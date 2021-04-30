/**
 *
 */
package common;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.*;

import sim_diasca.DataflowTypes;
import sim_diasca.DataflowTypes.ActivationPolicy;
import sim_diasca.DataflowTypes.InputPortsSpecification;
import sim_diasca.DataflowTypes.OutputPortsSpecification;
import sim_diasca.ProcessingUnit;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 *
 * Class defining how to handle incoming messages (received by the Java
 * interpreter) and which actions they must trigger (including sending back new
 * messages to the Erlang part).
 *
 * The key method describing the behavior of the message handler is the
 * handle_message function.
 *
 * To preserve genericity, any incoming message will be decomposed by the
 * communicator from a triplet with one leading technical element to a pair
 * formed by a title and a body (in this order), which will have had its
 * structure checked, i.e.:
 *
 *   original_message == ( message_header, message_title, message_body )
 *   filtered_message == MsgReceive( message_title, message_body )
 *
 * Then the message title is interpreted as an action to perform (it must match
 * the name of an available method), the message body containing all the data
 * required to perform it:
 *
 *   method_name, data_list = filtered_message
 */
public class MessageHandler extends TraceEmitter {

	public MessageHandler(BaseCommunicator communicator) {
		super(communicator);
	}


	/**
	 * Generic method handling all incoming messages and translating their
	 * title part automatically into an action (calling a method bearing the
	 * same name as this title).
	 * @param elements_msg
	 */
	public void handle_message(OtpErlangObject[] elements_msg) throws Exception {

		if ((!( elements_msg[1] instanceof OtpErlangAtom)) | elements_msg.length != 3) {
			this.sendException(new OtpErlangException(String.format(
				  "The message structure does not follow the convention: %s",
				  elements_msg.toString())));
		} else {
			MsgReceive msg_receive = TraceEmitter.communicator.filter(elements_msg);

			try {
				Method func = this.getClass().getMethod(
				  msg_receive.getMsg_title().toString(), Object[].class);
				func.invoke(this, msg_receive);

			} catch (Exception e) {
				this.sendException(e);
			}
		}
	}


	/**
	 * Defines what to do with the result of a request when successful.
	 * @param request_result
	 */
	public void requestReturn(ArrayList<?> request_result) {

		TraceEmitter.communicator.send(request_result, "request_completed");
	}


	/**
	 * Looks after the static declaration of the full description of all ports
	 * of a target processing unit.
	 *
	 * @param msg_receive
	 */
	public void getPortSpecifications(MsgReceive msg_receive)
		throws NoSuchMethodException, SecurityException, IllegalAccessException,
			   IllegalArgumentException, InvocationTargetException {

		String className = msg_receive.getClassName();

		try {
			// call static function
			Method func = Class.forName(className).getMethod(
			  msg_receive.getMsg_title().toString(), null);
			List<List<?>> port_specs = (List<List<?>>)func.invoke(null);

			List<InputPortsSpecification> input_port_specs =
				new ArrayList<InputPortsSpecification>();

			List<OutputPortsSpecification> output_port_specs =
				new ArrayList<OutputPortsSpecification>();

			for (Object i_port_specs : port_specs.get(0)) {
				input_port_specs.add((InputPortsSpecification) i_port_specs);
			}

			for (Object i_port_specs : port_specs.get(1)) {
				output_port_specs.add((OutputPortsSpecification) i_port_specs);
			}

			this.requestReturn(new ArrayList<>(Arrays.asList(input_port_specs,
				  output_port_specs)));

		} catch (ClassNotFoundException e) {
			this.requestReturn(new ArrayList<>(Arrays.asList(
				  "no_port_specifications_declared")));
		}
	}


	/**
	 * Looks after the static declaration of all the semantics used by the ports
	 * of a target processing unit.
	 *
	 * @param msg_receive
	 *
	 */
	public void getDeclaredSemantics(MsgReceive msg_receive) {

		String className = msg_receive.getClassName();

		try {
			// call static function
			Method func = Class.forName(className).getMethod(
			  msg_receive.getMsg_title().toString(), null);
			List<String> semantics = (List<String>)func.invoke(null);

			this.requestReturn(new ArrayList<>(Arrays.asList(semantics)));

		} catch (Exception e) {
			this.requestReturn(new ArrayList<>(Arrays.asList(
				  "no_semantics_declared")));
		}
	}


	/**
	 * Looks after the static declaration of all the types supported for the
	 * values borne by the ports of a target processing unit.
	 *
	 * @param msg_body
	 *
	 */
	public void getDeclaredTypes(MsgReceive msg_receive) {

		String className = msg_receive.getClassName();

		try {
			// call static function
			Method func = Class.forName(className).getMethod(
			  msg_receive.getMsg_title().toString(), null);
			List<?> types = (List<?>)func.invoke(null);

			this.requestReturn(new ArrayList<>(Arrays.asList(types)));

		} catch (Exception e) {
			this.requestReturn(new ArrayList<>(
				Arrays.asList("no_types_declared")));
		}
	}


	/**
	 * Instantiates a target processing unit with the construction parameters
	 * found in the incoming message.
	 *
	 * @param msg_body
	 */
	public void instantiateUnit(MsgReceive msg_receive) {
		String className = msg_receive.getClassName();

		try {
			Class cl = Class.forName(className);
			Constructor ct = cl.getConstructors()[0];
			Object o = ct.newInstance(msg_receive.GetParamConstructor());

			// Announces the creation of a processing unit in a trace:
			this.sendDebug(String.format(
				"Creating the processing unit %s of type %s from %s.",className,
				className));

			// Informs that the processing unit was successfully instantiated
			Method m = cl.getMethod("getUnitDescription", null);
			this.sendDebug(String.format(
				"Processing unit successfully created:  %s.", m.invoke(o, null)));

			// Saves this unit object and creates a reference on it
			int new_instance_id = InterpreterState.units_creation_counter;
			InterpreterState.processing_units.put(Integer.toString(
				new_instance_id), o);

			// Increments the ever-increasing global counter used to make unique IDs
			InterpreterState.units_creation_counter += 1;

			m = cl.getMethod("getActivation_policy", null);
			DataflowTypes.ActivationPolicy activation_policy =
				(ActivationPolicy) m.invoke(o, null);

		} catch (Exception e) {
			this.requestReturn(new ArrayList<>(Arrays.asList("no_types_declared")));
		}
		this.sendDebug(String.format(
			"Creating the processing unit %s of type %s from %s.", className,
			className));
	}


	/**
	 * Triggers the activate method of a target processing unit and taking into
	 * account the inputs found in the incoming message.
	 *
	 * Although the activate method of a processing unit does not take any
	 * parameter in input, the incoming message contains data which is used to
	 * update the state of the processing unit prior to activating it.
	 *
	 * These last actions are achieved through the call to a manage_activation
	 * method that wraps the call to activate.
	 *
	 * The input data taken by this manage_activation method are mainly, if not
	 * only, the context of the simulation and the statuses of the input ports
	 * (and of the port iterations, if any).
	 *
	 * The activate method is then called.
	 *
	 * Once completed, the manage_activation wrapper method returns the encoded
	 * version of its outputs, which are the values of the output ports that
	 * have been fed by the activate method. The encoding makes them ready to be
	 * sent to the native coupling infrastructure.
	 *
	 * @param msg_body
	 *
	 */
	public void activateUnit(MsgReceive msg_receive) throws OtpErlangRangeException {
		int instance_ID = ((OtpErlangInt)((OtpErlangTuple)
			msg_receive.getMsg_body()).elementAt(0)).intValue();

		ActivationData activation_data = msg_receive.getActivationData();

		// Calls the method managing the activation process:
		Object o = InterpreterState.processing_units.get(instance_ID);
		//ArrayList<?> encode_activation_results = o.manage_activation(activation_data);
		Map<String, Object> encode_activation_results =
			((ProcessingUnit) o).manageActivation(activation_data);

		this.requestReturn(new ArrayList<>(Arrays.asList(encode_activation_results)));
	}


	/**
	 * Triggers the deletion method of a target processing unit.
	 * @param msg_body
	 */
	public void deleteUnit(int msg_body) {
		InterpreterState.processing_units.remove(msg_body);
	}

}
