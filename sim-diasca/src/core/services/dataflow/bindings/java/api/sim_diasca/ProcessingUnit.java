/**
 * 
 */
package sim_diasca;

import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import common.ActivationData;
import sim_diasca.DataflowTypes.InputPortsSpecification;
import sim_diasca.DataflowTypes.OutputPortsSpecification;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 * 
 * Base class for all actual dataflow processing units.
 *
 * All the processing units defined by the users of this API must inherit from
 * this class. The API is defined as the set of methods bound to this class
 * (and actually also to its parent, the (abstract) Block class, which cannot
 * be used directly).
 *
 * The life of a processing unit mainly boils down to:
 *
 * - first being created, its construction being driven thanks to the
 *   classical __init__ method, allowing to define what are its input and
 *   output ports, and how it shall be activated (typically whenever at least
 *   one of its input ports is set, or if and only if all of them are set)
 *
 * - then being activated (any number of times), resulting on its associated
 *   computations to be performed, fed by its own state and by the values (if
 *   any) set in its input ports, and possibly feeding in turn its output ports
 */
public abstract class ProcessingUnit extends Block {
	
	private DataflowTypes.ActivationPolicy activation_policy;
	
	public DataflowTypes.ActivationPolicy getActivation_policy() {
		return activation_policy;
	}

	public void setActivation_policy(DataflowTypes.ActivationPolicy activation_policy) {
		this.activation_policy = activation_policy;
	}

	/**
	 * The constructor of all data flow processing units.
	 * 
	 * It allows to define the name of the unit, on which policy it should be
	 * activated, and the full specifications of the input and output ports it comprises.
	 * 
	 * @param name
	 * @param input_port_specs
	 * @param output_port_specs
	 */
	public ProcessingUnit(String block_name, DataflowTypes.ActivationPolicy activation_p, 
			List<InputPortsSpecification> input_port_specs,
			List<OutputPortsSpecification> output_port_specs) throws OtpErlangRangeException {
		
		super(block_name, input_port_specs, output_port_specs);
		setActivation_policy(activation_p);
	}
	
	/**
	 * The destructor of all dataflow processing units.
	 * 
	 * It sends a trace in order to notify that the instance has indeed been
	 * deleted and that no reference should remain on the instance. Memory 
	 * being theoretically freed by the garbage collector, there should be no
	 *  more memory usage due to this instance. (This notification might help debug memory issues.)
	 */
	public void finalize() {
		this.sendDebug(String.format("The Python processing unit %s is being deleted", this.getName()));
	}
	
	/**
	 * Interface layer between the Erlang and Java activate/1 methods. 
	 * 
	 * Called by the Erlang side; prepares for, calls the activate/1 method, and handles its result.
	 * @param activation_data
	 */
	public Map<String, Object> manageActivation(ActivationData activation_data) throws OtpErlangRangeException {
					
		// Translates and binds to the local state the data received from Erlang
		this.prepareActivation(activation_data.getSimulation_state(), activation_data.getInput_ports_data(),
				activation_data.getInput_port_iterations_data());
		
		// Trace informing that the user-defined activate/1 method can start
		this.sendDebug(String.format("Entering the core python activation method for the unit: %s", this.getName()));
		
		// Calls the user-defined activate/1 method
		this.activate();
		
		// Replaces the internal reference to the (potentially big) list of
        // activation results by a locally defined one, so that as soon as this
        // method has ended, no reference remains and memory is freed
		Map<String, Object> activation_results = this.activation_results;
		this.activation_results.clear();
		
		// Activation results were already encoded by the functions of the API, so they can be return as is:
		return activation_results;
	}
	
	/**
	 * Binds all the data requestable through the API to the state of the current instance.
	 * 
	 * Internal method decoding the data received from Erlang and binding 
	 * them to the local state of the processing unit. The state imported from Erlang,
	 * which is to update the corresponding parts of the Java one, is separated in three parts (arguments):
	 * 
	 *   - *simulation_state* is about everything not related to any port:
	 *      currently, it stores only the tick offset (integer) and the
	 *      simulation date (2-tuple of the form ((Yr,Mon,Day),(Hr,Min,Sec))).
	 *      
	 *   - *input_ports_data* stores pairs of the form (port_name,port_value)
	 *   
	 *   - *input_port_iterations_data* stores the up-to-date structural
	 *      metadata associated to input port iterations, under the form of
	 *      3-tuples storing the base name, the multiplicity and the list of
	 *      indexes (this latter element is maybe useless ?).
	 * @param simulation_state 
	 * @param input_port_data
	 * @param input_port_iterations_data
	 */
	private void prepareActivation(OtpErlangTuple simulation_state, OtpErlangList input_port_data,
			OtpErlangList input_port_iterations_data) throws OtpErlangRangeException {
		
		// Copies the pieces of simulation state to update:
		this.setSimulationState(simulation_state);
				
		// Decodes the statuses of input ports received from Erlang
		for (OtpErlangObject input : input_port_data) {
			OtpErlangTuple tuple_input = (OtpErlangTuple) input;
			String input_port_name = ((OtpErlangString) tuple_input.elementAt(0)).stringValue();
			
			if(!this.getInput_ports().containsKey(input_port_name)) {
				this.setInput_ports(input_port_name, new DataflowTypes().new InputPort(input_port_name));
			}
			
			OtpErlangTuple encoded_status = (OtpErlangTuple) tuple_input.elementAt(1);
			String status = ((OtpErlangString) encoded_status.elementAt(0)).stringValue();
			String value = ((OtpErlangString) encoded_status.elementAt(1)).stringValue();
			this.getInput_ports().get(input_port_name).update(new String[]{status,value});			
		}
		
		for (OtpErlangObject input : input_port_iterations_data) {
			OtpErlangTuple tuple_input = (OtpErlangTuple) input;
			String input_port_iterations_name = ((OtpErlangString) tuple_input.elementAt(0)).stringValue();
			OtpErlangObject multiplicity = (OtpErlangObject) tuple_input.elementAt(1);
			OtpErlangList indexes = (OtpErlangList) tuple_input.elementAt(2);
			
			this.getInput_port_iterations().get(input_port_iterations_name).update(multiplicity, indexes);
		}
	}
	
	/**
	 * Declares the activate/1 method that must be overridden by the child (user-defined) units.
	 */
	public abstract void activate() throws OtpErlangRangeException;
}
