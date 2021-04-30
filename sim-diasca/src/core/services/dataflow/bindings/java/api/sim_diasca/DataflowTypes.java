/**
 * 
 */
package sim_diasca;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import common.TraceEmitter;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 *
 */
public class DataflowTypes {
	
	public Class[] types = new Class[]{String.class, String.class};
	
	/**
	 * Enumeration matching the activation policies available in Sim-Diasca.
	 * The activation policy governing the condition(s) to match before triggering
	 * the evaluation of the 'activate' method of a processing unit.
	 */
	public enum ActivationPolicy {
		ACTIVATE_ON_NEW_SET,
		ACTIVATE_WHEN_ALL_SET,
		CUSTOM_ACTIVATION,
	}
	
	/**
	 * Enumeration matching the constraints available in Sim-Diasca.
	 * The available constraints, against which any port value can be checked:
	 * Note: these must match the list of constraints in Sim-Diasca, which can be
	 * found in class_DataflowBlock_functions.hrl:validate_constraints/2 as Erlang atoms.
	 */
	public enum PortConstraints {
		NON_NULL,
		STRICTLY_NEGATIVE,
		NEGATIVE,
		STRICTLY_POSITIVE,
		POSITIVE,
		IN,					// to be used as ( IN, list )
		BETWEEN,			// to be used as ( BETWEEN, x, y )
		LOWER_THAN,			// to be used as ( LOWER_THAN, x )
		GREATER_THAN, 		// to be used as ( GREATER_THAN, x )
	}
	
	/**
	 * User-defined specifications describing an input port (iterated or not).
	 *
	 */
	public class InputPortsSpecification {
		
		private String name;
		private String comment;
		private Boolean is_iteration;
		private String value_semantics;
		private String value_unit;
		private String value_type_description;
		private List<PortConstraints> value_constraints;		
		
		/**
		 * Initial specification of all the characteristics of an input port.
		 * @param name
		 * @param comment
		 * @param is_iteration
		 * @param value_semantics
		 * @param value_unit
		 * @param value_type_description
		 * @param value_constraints
		 */
		public InputPortsSpecification(String name, String comment, Boolean is_iteration, String value_semantics,
				String value_unit, String value_type_description, List<PortConstraints> value_constraints) {
			super();
			this.name = name;
			this.comment = comment;
			this.is_iteration = is_iteration;
			this.value_semantics = value_semantics;
			this.value_unit = value_unit;
			this.value_type_description = value_type_description;
			this.value_constraints = value_constraints;
		}
		
		/**
		 * Initial specification of all the characteristics of an input port.
		 * @param name
		 * @param comment
		 * @param value_semantics
		 * @param value_unit
		 * @param value_type_description
		 * @param value_constraints
		 */
		public InputPortsSpecification(String name, String comment, String value_semantics, String value_unit,
				String value_type_description, List<PortConstraints> value_constraints) {
			super();
			this.name = name;
			this.comment = comment;
			this.is_iteration = false;
			this.value_semantics = value_semantics;
			this.value_unit = value_unit;
			this.value_type_description = value_type_description;
			this.value_constraints = value_constraints;
		}

		// Get/Set
		public String getName() {
			return name;
		}
		public void setName(String name) {
			this.name = name;
		}
		public Boolean getIs_iteration() {
			return is_iteration;
		}
		public void setIs_iteration(Boolean is_iteration) {
			this.is_iteration = is_iteration;
		}

		/**
		 * Transforms an InputPortSpecification object map.
		 */
		public Map<String, Object> encode() {
			Map<String, Object> map = new HashMap<>();
			map.put("name", this.name);
			map.put("comment", this.comment);
			map.put("is_iteration", this.is_iteration);
			map.put("value_semantics", this.value_semantics);
			map.put("value_unit", this.value_unit);
			map.put("value_type_description", this.value_type_description);
			map.put("value_constraints", this.value_constraints);
			return map;
		}
	}
	
	/**
	 * User-defined specifications describing an input port (iterated or not).
	 *
	 */
	public class OutputPortsSpecification {
		
		private String name;
		private String comment;
		private Boolean is_iteration;
		private Boolean produces_result;
		private String value_semantics;
		private String value_unit;
		private String value_type_description;
		private List<PortConstraints> value_constraints;		
		
		// Get/Set		
		public String getName() {
			return name;
		}
		public void setName(String name) {
			this.name = name;
		}
		public Boolean getIs_iteration() {
			return is_iteration;
		}
		public void setIs_iteration(Boolean is_iteration) {
			this.is_iteration = is_iteration;
		}

		/**
		 * Initial specification of all the characteristics of an output port.
		 * @param name
		 * @param comment
		 * @param is_iteration
		 * @param produces_result
		 * @param value_semantics
		 * @param value_unit
		 * @param value_type_description
		 * @param value_constraints
		 */
		public OutputPortsSpecification(String name, String comment, Boolean is_iteration, Boolean produces_result,
				String value_semantics, String value_unit, String value_type_description,
				List<PortConstraints> value_constraints) {
			super();
			this.name = name;
			this.comment = comment;
			this.is_iteration = is_iteration;
			this.produces_result = produces_result;
			this.value_semantics = value_semantics;
			this.value_unit = value_unit;
			this.value_type_description = value_type_description;
			this.value_constraints = value_constraints;
		}

		/**
		 * Initial specification of all the characteristics of an output port.
		 * @param name
		 * @param comment
		 * @param value_semantics
		 * @param produces_result
		 * @param value_unit
		 * @param value_type_description
		 * @param value_constraints
		 */
		public OutputPortsSpecification(String name, String comment, String value_semantics, Boolean produces_result,
				String value_unit, String value_type_description, List<PortConstraints> value_constraints) {
			super();
			this.name = name;
			this.comment = comment;
			this.produces_result = produces_result;
			this.value_semantics = value_semantics;
			this.value_unit = value_unit;
			this.value_type_description = value_type_description;
			this.value_constraints = value_constraints;
		}

		/**
		 * Initial specification of all the characteristics of an output port.
		 * @param name
		 * @param comment
		 * @param is_iteration
		 * @param value_semantics
		 * @param value_unit
		 * @param value_type_description
		 * @param value_constraints
		 */
		public OutputPortsSpecification(String name, String comment, Boolean is_iteration, String value_semantics,
				String value_unit, String value_type_description, List<PortConstraints> value_constraints) {
			super();
			this.name = name;
			this.comment = comment;
			this.is_iteration = is_iteration;
			this.value_semantics = value_semantics;
			this.value_unit = value_unit;
			this.value_type_description = value_type_description;
			this.value_constraints = value_constraints;
		}

		/**
		 * Initial specification of all the characteristics of an output port.
		 * @param name
		 * @param comment
		 * @param value_semantics
		 * @param value_unit
		 * @param value_type_description
		 * @param value_constraints
		 */
		public OutputPortsSpecification(String name, String comment, String value_semantics, String value_unit,
				String value_type_description, List<PortConstraints> value_constraints) {
			super();
			this.name = name;
			this.comment = comment;
			this.value_semantics = value_semantics;
			this.value_unit = value_unit;
			this.value_type_description = value_type_description;
			this.value_constraints = value_constraints;
		}

		/**
		 * Transforms an OutputPortSpecification object map.
		 */
		public Map<String, Object> encode() {
			Map<String, Object> map = new HashMap<>();
			map.put("name", this.name);
			map.put("comment", this.comment);
			map.put("produces_result", this.produces_result);
			map.put("is_iteration", this.is_iteration);
			map.put("value_semantics", this.value_semantics);
			map.put("value_unit", this.value_unit);
			map.put("value_type_description", this.value_type_description);
			map.put("value_constraints", this.value_constraints);
			return map;
		}
	}
	
	/**
	 * Base class for input and output ports
	 */
	public class GenericPort extends TraceEmitter {
		
		protected String name;
		protected String status;
		protected String value;		

		/**
		 * Constructor of a generic port.
		 * @param name
		 * @param status
		 * @param value
		 */
		public GenericPort(String name, String status, String value) {
			super(communicator);
			this.name = name;
			this.status = status;
			this.value = value;
		}

		/**
		 * Constructor of a generic port.
		 * @param name
		 * @param status
		 */
		public GenericPort(String name, String status) {
			super(communicator);
			this.name = name;
			this.status = status;
			this.value = null;
		}
		
		/**
		 * Constructor of a generic port.
		 * @param name
		 */
		public GenericPort(String name) {
			super(communicator);
			this.name = name;
			this.status = null;
			this.value = null;
		}
		
		// Get/set		
		public String getName() {
			return name;
		}
		public void setName(String name) {
			this.name = name;
		}

		/**
		 * Updates the status of a port with a status received from Sim-Diasca.
		 * @param encoded_status
		 */
		public void update(String[] encoded_status) {
			if(encoded_status[0].equals("unset")) {
				this.status = "unset";
				this.value = null;
			} else if (encoded_status.length == 2 && encoded_status[0].equals("set")) {
				this.status = "set";
				this.value = encoded_status[1];
			} else {
				this.sendError(String.format("Bad input port status received from Erlang while updating: %s", 
						encoded_status.toString()));
			}
		}
	}
	
	/**
	 * Definition of an input port, actually a subset of its Erlang counterpart, made by taking only 
	 * the data relevant for the computations of an 'activate' method
	 */
	public class InputPort extends GenericPort {

		public InputPort(String name, String status, String value) {
			super(name, status, value);
		}
		public InputPort(String name, String status) {
			super(name, status, null);
		}
		public InputPort(String name) {
			super(name);
		}
		@Override
		public String toString() {
			return String.format("InputPort : %", this.name);
		}		
	}
	
	/**
	 * Definition of an output port, actually a subset of its Erlang counterpart, made by taking only 
	 * the data relevant for the computations of an 'activate' method
	 */
	public class OutputPort extends GenericPort {

		public OutputPort(String name, String status, String value) {
			super(name, status, value);
		}
		public OutputPort(String name, String status) {
			super(name, status, null);
		}
		public OutputPort(String name) {
			super(name);
		}
		@Override
		public String toString() {
			return String.format("OutputPort : %", this.name);
		}		
	}
	
	/**
	 * Base class for input and output port iterations
	 */
	public class GenericPortIteration extends TraceEmitter {

		protected String base_name;
		private int port_count;
		private OtpErlangTuple bounds;
		private OtpErlangList indexes = null;
		private OtpErlangObject iteration_spec;
		
		/**
		 * Constructor of a generic port iteration.
		 * @param communicator
		 * @param base_name
		 * @param iteration_spec
		 * @param indexes
		 * @throws OtpErlangRangeException 
		 */
		public GenericPortIteration(String base_name, OtpErlangObject iteration_spec, OtpErlangList indexes) 
				throws OtpErlangRangeException {
			super(communicator);
			this.base_name = base_name;
			this.indexes = indexes;
			this.iteration_spec = iteration_spec;
			
			if(iteration_spec instanceof OtpErlangTuple) {
				OtpErlangTuple tuple_iteration_spec = (OtpErlangTuple) iteration_spec;
				this.port_count = ((OtpErlangInt)tuple_iteration_spec.elementAt(0)).intValue();
				if(tuple_iteration_spec.elementAt(1) instanceof OtpErlangTuple) {
					this.bounds = (OtpErlangTuple)tuple_iteration_spec.elementAt(1);
				} else {
					this.bounds = new OtpErlangTuple(new OtpErlangObject[] {new OtpErlangInt(0), 
							new OtpErlangTuple(tuple_iteration_spec).elementAt(1)});
				}
			} else if (iteration_spec instanceof OtpErlangInt) {
				this.port_count = ((OtpErlangInt)iteration_spec).intValue();
				this.bounds = new OtpErlangTuple(new OtpErlangObject[] {new OtpErlangInt(0), null});
			} else {
				this.port_count = 0;
				this.bounds = new OtpErlangTuple(new OtpErlangObject[] {new OtpErlangInt(0), null});
			}		

			this.indexes = indexes;
				
			if(this.indexes != null && this.port_count != this.indexes.arity()) {
				this.sendError(String.format("Initially, the list of indexes (length %s) does not match "
						+ "the declared multiplicity (%s) for the input port iteration %s.", 
						this.indexes.arity(), this.port_count, this.base_name));
			}			
		}
		
		// Get/Set
		public OtpErlangList getIndexes() {
			return indexes;
		}
		public void setIndexes(OtpErlangList indexes) {
			this.indexes = indexes;
		}				
		public int getPort_count() {
			return port_count;
		}
		public void setPort_count(int port_count) {
			this.port_count = port_count;
		}

		/**
		 * Updates the port iteration according to the data received from Erlang
		 * @param erlang_multiplicity
		 * @param erlang_indexes
		 * @throws OtpErlangRangeException 
		 */
		public void update(OtpErlangObject multiplicity, OtpErlangList list_indexes) throws OtpErlangRangeException {
			
			if(multiplicity instanceof OtpErlangTuple && list_indexes.arity() == 2) {
				OtpErlangTuple tuple_multiplicity = (OtpErlangTuple) multiplicity;
				
				// The count of member ports is just copied
				this.port_count = ((OtpErlangInt)tuple_multiplicity.elementAt(0)).intValue();
				
				// The bounds are stored as tuple of length 2 which is also copied
				if(tuple_multiplicity.elementAt(1) instanceof OtpErlangTuple && 
						((OtpErlangTuple)tuple_multiplicity.elementAt(1)).arity() == 2) {
					OtpErlangTuple bounds_tuple = (OtpErlangTuple)tuple_multiplicity.elementAt(1);
					this.bounds = bounds_tuple;
				} else {
					this.sendError(String.format("Ill-formed bounds tuple received from Erlang for the port "
							+ "iteration %s: %s ",
							this.base_name, bounds.toString()));
				}
			} else {
				this.sendError(String.format("Ill-formed multiplicity received from Erlang while updating the "
						+ "port iteration %s: %s",
						this.base_name, multiplicity.toString()));
			}
			
			this.indexes = list_indexes;
		}
	}
	
	/**
	 * Definition of an input port, actually a subset of its Erlang counterpart, made by taking only the data relevant 
	 * for the computations of an 'activate' method
	 */
	public class InputPortIteration extends GenericPortIteration{

		public InputPortIteration(String base_name, OtpErlangObject iteration_spec, OtpErlangList indexes) 
				throws OtpErlangRangeException {
			super(base_name, iteration_spec, indexes);
		}

		@Override
		public String toString() {
			return "InputPortIteration " + this.base_name;
		}		
	}
	
	public class OutputPortIteration extends GenericPortIteration{

		public OutputPortIteration(String base_name, OtpErlangObject iteration_spec, OtpErlangList indexes) 
				throws OtpErlangRangeException {
			super(base_name, iteration_spec, indexes);
		}
		
		@Override
		public String toString() {
			return "OutputPortIteration " + this.base_name;
		}
	}
	
	/**
	 * Definition of a channel value, which is an enrichment of the simple value borne by a port: just aside the value, 
	 * inside a tuple, we join some meta data used by Sim-Diasca for checking that inter-block communications make sense.
	 * This tuple containing a value and its meta data is called a channel value.
	 */
	public class ChannelValue{
		private double value;
		private String value_semantics;
		private String value_unit;
		private String value_type;
		
		/**
		 * Creates a new channel value from a basic value and some metadata: the semantics revealing what the value
		 * represents to humans, the physical unit with which it should be accompanied and its computational type.
		 * @param actual_impact
		 * @param value_semantics
		 * @param value_unit
		 * @param value_type
		 */
		public ChannelValue(double actual_impact, String value_semantics, String value_unit, String value_type) {
			super();
			this.value = actual_impact;
			this.value_semantics = value_semantics;
			this.value_unit = value_unit;
			this.value_type = value_type;
		}				
	}
		
	public class ErlangMultiplicity {
		
		private int port_count = -1;
		private int min_bound = -1;
		private int max_bound = -1;
		
		/**
		 * @param port_count
		 * @param min_bound
		 * @param max_bound
		 */
		public ErlangMultiplicity(int port_count, int min_bound, int max_bound) {
			super();
			this.port_count = port_count;
			this.min_bound = min_bound;
			this.max_bound = max_bound;
		}

		// Get/Set
		public int getPort_count() {
			return port_count;
		}
		public void setPort_count(int port_count) {
			this.port_count = port_count;
		}
		public int getMin_bound() {
			return min_bound;
		}
		public void setMin_bound(int min_bound) {
			this.min_bound = min_bound;
		}
		public int getMax_bound() {
			return max_bound;
		}
		public void setMax_bound(int max_bound) {
			this.max_bound = max_bound;
		}	
		
		@Override
		public String toString() {
			return "ErlangMultiplicity [port_count=" + port_count + ", min_bound=" + min_bound + ", max_bound="
					+ max_bound + "]";
		}
	}
}

