/**
 * 
 */
package sim_diasca;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import common.TraceEmitter;
import sim_diasca.DataflowTypes.ChannelValue;
import sim_diasca.DataflowTypes.InputPort;
import sim_diasca.DataflowTypes.InputPortIteration;
import sim_diasca.DataflowTypes.InputPortsSpecification;
import sim_diasca.DataflowTypes.OutputPort;
import sim_diasca.DataflowTypes.OutputPortIteration;
import sim_diasca.DataflowTypes.OutputPortsSpecification;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 * 
 * Class representing dataflow blocks generically.
 * 
 * All the processing units defined by users of the API must inherit from ProcessingUnit, not from this class.
 * This class is an abstract one and thus cannot be instantiated as such, even though it contains a large part of
 * the methods of the API.
 */
public class Block extends TraceEmitter{
	
	private String name;
	private int tick_offset = 0;
	private int simulation_date_year;

	private List<InputPortsSpecification> input_port_specs = new ArrayList<InputPortsSpecification>();
	private Map<String, InputPort> input_ports = new HashMap<String, InputPort>();
	private Map<String, InputPortIteration> input_port_iterations = new HashMap<String, InputPortIteration>();

	private List<OutputPortsSpecification> output_port_specs = new ArrayList<OutputPortsSpecification>();
	private Map<String, OutputPort> output_ports = new HashMap<String, OutputPort>();
	private Map<String, OutputPortIteration> output_port_iterations = new HashMap<String, OutputPortIteration>();
    
    protected Map<String, Object> activation_results = new HashMap<String, Object>();
    
    protected static sim_diasca.DataflowTypes dataflow;
    
    // Get/Set    
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	public int getTick_offset() {
		return tick_offset;
	}

	public void setTick_offset(int tick_offset) {
		this.tick_offset = tick_offset;
	}
	public int getSimulation_date_year() {
		return simulation_date_year;
	}

	public void setSimulation_date_year(int year) {
		this.simulation_date_year = year;
	}
	public List<InputPortsSpecification> getInput_port_specs() {
		return input_port_specs;
	}

	public void setInput_port_specs(List<InputPortsSpecification> input_port_specs) {
		this.input_port_specs = input_port_specs;
	}
	public Map<String, InputPort> getInput_ports() {
		return input_ports;
	}

	public void setInput_ports(String name, InputPort input_ports) {
		this.input_ports.put(name, input_ports);
	}
	
	public Map<String, InputPortIteration> getInput_port_iterations() {
		return input_port_iterations;
	}
	public void setInput_port_iterations(String name, InputPortIteration input_port_iterations) {
		this.input_port_iterations.put(name, input_port_iterations);
	}
	public List<OutputPortsSpecification> getOutput_port_specs() {
		return output_port_specs;
	}

	public void setOutput_port_specs(List<OutputPortsSpecification> output_port_specs) {
		this.output_port_specs = output_port_specs;
	}

	/**
	 * @param communicator
	 * @param name
	 * @param input_port_specs
	 * @param output_port_specs
	 */
	public Block(String name,  List<InputPortsSpecification> input_port_specs,  
			List<OutputPortsSpecification> output_port_specs) throws OtpErlangRangeException {
		super(communicator);
		this.name = name;
		this.setInput_port_specs(input_port_specs);
		this.setOutput_port_specs(output_port_specs);
		
		dataflow = new DataflowTypes();
		
		// Builds the list of input ports from the specifications:
		for (InputPortsSpecification input_ports_specification : this.input_port_specs) {
			String name_ips = input_ports_specification.getName();
			Boolean iteration_ips = input_ports_specification.getIs_iteration();
			
			int it_int = iteration_ips ? 1 : 0;
			
			if(!iteration_ips) {
				this.input_ports.put(name_ips, dataflow.new InputPort(name_ips));
			} else {
				InputPortIteration ipi = dataflow.new InputPortIteration(name_ips, 
						new OtpErlangTuple(new OtpErlangObject[] {new OtpErlangInt(it_int), null}), null);
				this.input_port_iterations.put(name_ips, ipi);
				
				if(ipi.getIndexes() != null) {
					for (OtpErlangObject idx : ipi.getIndexes()) {
						String iterated_name = this.getIteratedPortName(name_ips, ((OtpErlangInt)idx).intValue());						
						input_ports.put(iterated_name, dataflow.new InputPort(iterated_name));
					}
				}
			}
		}
		
		// Builds the list of output ports from the specifications
		for (OutputPortsSpecification output_ports_specification : this.output_port_specs) {
			String name_ips = output_ports_specification.getName();
			Boolean iteration_ips = output_ports_specification.getIs_iteration();
			
			int it_int = iteration_ips ? 1 : 0;
			
			if(!iteration_ips) {
				this.output_ports.put(name_ips, dataflow.new OutputPort(name_ips));
			} else {
				OutputPortIteration ipi = dataflow.new OutputPortIteration(name_ips, 
						new OtpErlangTuple(new OtpErlangObject[] {new OtpErlangInt(it_int), null}), null);
				this.output_port_iterations.put(name_ips, ipi);
				
				if(ipi.getIndexes() != null) {
					for (OtpErlangObject idx : ipi.getIndexes()) {
						String iterated_name = this.getIteratedPortName(name_ips, ((OtpErlangInt)idx).intValue());						
						output_ports.put(iterated_name, dataflow.new OutputPort(iterated_name));
					}
				}
			}
		}
	}

	/**
	 * Calls the Sim-Diasca convention linking a port name to its iteration name
	 * @param name_ips
	 * @param idx
	 */
	private String getIteratedPortName(String name_ips, int idx) {
		String ports_base_name = this.input_port_iterations.get(name_ips).base_name;		
		return String.format("%s_itrated_%s", ports_base_name, Integer.toString(idx));
	}
	
	/**
	 * Setter for the part of the state of a processing unit that must be synchronized with the Erlang process
	 * @param simulation_state 
	 */
    public void setSimulationState(OtpErlangTuple simulation_state) throws OtpErlangRangeException {
    	this.setTick_offset(((OtpErlangInt) simulation_state.elementAt(0)).intValue());
    	
    	OtpErlangTuple tuple_date = (OtpErlangTuple) ((OtpErlangTuple) simulation_state.elementAt(1)).elementAt(0);
    	this.setSimulation_date_year(((OtpErlangInt) tuple_date.elementAt(0)).intValue());
    }
    
    /**
     * Gets the value present on a single port (also checking its status)
     * @param input_port_name
     * @return
     */
    public String getInputPortValue(String input_port_name) {
    	// Gets the target input port identified by its name
    	InputPort ip = this.input_ports.get(input_port_name);
    	
    	if(ip.status.equals("set")) {
    		return ip.value;
    	} else if (ip.status.equals("unset")) {
			this.sendError(String.format("Requested the value of the input port %s, but this one is unset.", 
					input_port_name));
		} else {
			this.sendError(String.format("Unknown status for the input port %s: %s", input_port_name, ip.status));
		}
    	return null;
    }
    
    /**
     * Gets all the values borne by the members of an input port iteration
     * @param iteration_name
     */
    public String[] getAllInputIterationValues(String iteration_name) throws OtpErlangRangeException {
    	// Gets the input port iteration identified by its name
    	InputPortIteration input_port_iteration = this.input_port_iterations.get(iteration_name);
    	
    	// Gets the values of each member of the input port iteration and gathers
        // them inside a list, while detecting all 'unset' ports
    	String[] port_values = new String[input_port_iteration.getIndexes().arity()];
    	Boolean unset_found = false;
    	
    	for (OtpErlangObject idx : input_port_iteration.getIndexes()) {
			String iterated_name = this.getIteratedPortName(iteration_name, ((OtpErlangInt)idx).intValue());
			InputPort input_port = this.input_ports.get(iterated_name);
			
			if(input_port.status.equals("unset")) {
				unset_found = true;
			}
			
			port_values[((OtpErlangInt)idx).intValue()] = input_port.value;
		}
    	
    	// Sends a warning message if some ports have been found to be 'unset'
    	if(unset_found) {
    		this.sendWarning(String.format("In input port iteration %s, unset ports were encountered while "
    				+ "requesting all values.", 
    				iteration_name));
    	}
    	
    	// Returns the list of port values:
        return port_values;
    }
    
    /**
     * Creates a look-alike of channel value, actually just gathering the pieces of information needed to 
     * create one in the Erlang part of Sim-Diasca
     * @param actual_impact
     * @param value_semantics
     * @param value_unit
     * @param value_type
     * @return
     */
    public ChannelValue createChannelValue(double actual_impact, String value_semantics, String value_unit, 
    		String value_type) {
    	return Block.dataflow.new ChannelValue(actual_impact, value_semantics, value_unit, value_type);
    }
    
    /**
     * Creates a list of look-likes of channel values, by simply reusing the method described above
     * @param values
     * @param value_semantics
     * @param value_unit
     * @param value_type
     * @return
     */
    public ChannelValue[] createChannelsValues(int[] values, String value_semantics, String value_unit, 
    		String value_type) {
    	ChannelValue[] channels_values = new ChannelValue[values.length];
    	
    	for (int i = 0; i < values.length; i++) {
			channels_values[i] = dataflow.new ChannelValue(values[i], value_semantics, value_unit, value_type);
		}
    	
    	return channels_values;
    }
    
    /**
     * Sets a channel value on an output port identified by its name and encodes this name at the same time for 
     * a further sending to Erlang
     * @param output_port_name
     * @param channel_value
     */
    public void setOutputPortValue(String output_port_name, ChannelValue channel_value) {
    	this.activation_results.put(output_port_name, channel_value);
    }
    
    /**
     * Sets several channel values to a list of output ports (according the orderings of both lists, 
     * supposedly matching)
     * @param output_port_values
     */
    public void setOutputPortValues(Map<String, ChannelValue> output_port_values) {
    	
		Set<String> cles = output_port_values.keySet();
		Iterator<?> it = cles.iterator();
		
		while(it.hasNext()) {
			Object key = it.next();
			this.setOutputPortValue(key.toString(), output_port_values.get(key));
		}
    }
    
    /**
     * Sets the channel values of all members of an output port iteration
     * @param iteration_name
     * @param channel_value
     */
    public void setAllOutputIterationValues(String iteration_name, ChannelValue[] channel_values) 
    		throws OtpErlangRangeException {
    	// Gets the output port iteration
    	OutputPortIteration output_port_iteration = this.output_port_iterations.get(iteration_name);
    	
    	// Checks that the list of channel values matches the port count of the iteration
    	int port_count = output_port_iteration.getPort_count();
    	int number_of_values = channel_values.length;
    	
    	if(number_of_values < port_count) {
    		this.sendError(String.format("Trying to feed the ports of the output iteration %s with too few "
    				+ "channel values (%s<%s).",
    				iteration_name, number_of_values, port_count));
    	} else if(number_of_values > port_count){
    		this.sendError(String.format("Trying to feed the ports of the output iteration %s with too many "
    				+ "channel values (%s<%s).",
    				iteration_name, number_of_values, port_count));
    	}
    	
    	// Sets the channel values on the corresponding member ports
    	for (OtpErlangObject idx : output_port_iteration.getIndexes()) {
			for (int i = 0; i < number_of_values; i++) {
				this.setOutputPortValue(this.getIteratedPortName(iteration_name, ((OtpErlangInt)idx).intValue()),
						channel_values[i]);				
			}
		}    	
    }
}
