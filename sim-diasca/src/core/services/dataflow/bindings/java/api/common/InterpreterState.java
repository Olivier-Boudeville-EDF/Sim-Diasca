/**
 * 
 */
package common;

import java.util.Map;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 * 
 * Container class (never instantiated) keeping track of the whole state of its interpreter, 
 * notably in terms of instantiated objects.
 */
public final class InterpreterState {
	// Unique communicator, in charge of performing the actual communications with the (Erlang) core:
	static ErlangCommunicator global_communicator;
	
	// Unique message handler object defining the actions corresponding to all messages possibly received:
	static MessageHandler global_message_handler;
	
	// Unique dictionary of all the processing units instantiated and persisting in the current interpreter, 
	// along with their creation counter:
	static int units_creation_counter = 0;
	static Map<String, Object> processing_units;
	
	// Unique dictionary of all the unit managers instantiated and persisting in the current interpreter, 
	// along with their creation counter:
	static int managers_creation_counter = 0;
	static Map<?, ?> unit_managers;
}
