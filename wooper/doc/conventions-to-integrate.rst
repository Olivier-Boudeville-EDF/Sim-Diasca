- constructors, destructors shall never be explicitly exported by the user: due to their nature, they will be automatically exported
- methods must be (explicitly) exported; this has no link with the scope of a method (public, private, etc.), the problem is that, in the example below, getName/1 shall be a request, whereas nested_in_request/1 shall be an helper function:

% Returns the name of this instance.
%
% (const request)
%
-spec getName( wooper:state() ) -> request_return( name() ).
getName( State ) ->
	nested_in_request( State ).


% (helper)
nested_in_request( State ) ->
	wooper:return_state_result( State, ?getAttr(name) ).

Only the intent of the developer, once specified, allows to tell that nested_in_request/1 should not be considered as a method; so the following attributes shall be used to discriminate:

  -wooper_request_export([...]).
  -wooper_oneway_export([...]).
  -wooper_static_export([...]).

(this is most probably a better convention than requesting the developer to call wooper:return_state_result/2 and friends directly from the body of a given method)

 ==== OR, preferred: ====

Following WOOPER method terminators have been defined:

- wooper:return_state_result/2 for requests
- (optional: wooper:return_result/1 for const requests)

- wooper:return_state_only/1 for oneways
- (optional: wooper:return/0 for const oneways)

- wooper:return_static/1 for static methods


Variation (deemed less clear):

- wooper:request_return/2 for requests
- (optional: wooper:request_return/1 for const requests)

- wooper:oneway_return/1 for oneways
- (optional: wooper:return/0 for const oneways)

- wooper:oneway_return/1 for static methods


Optionally, on a per-class basis, the developer may opt for stricter method declarations, by exporting them explicitly, like in::

 -wooper_request_export([...]).
 -wooper_oneway_export([...]).
 -wooper_static_export([...]).


In this case, *all* methods (and only them) shall be listed in these declarations; notably, any method found that was not declared will result in a compilation error.




All clauses of a method must end with a call to its corresponding WOOPER method terminator (ex: for a request, it shall be wooper:return_state_result/2) directly from its body. Ex:

% Returns the name of this instance.
%
% (const request)
%
-spec getName( wooper:state() ) -> request_return( name() ).
getName( State ) ->
	Name = nested_in_request( State ),
	wooper:return_state_result( State, Name ).


% (helper)
nested_in_request( State ) ->
	?getAttr(name).

It allows to auto-export methods (otherwise the previous nested_in_request/1 would be considered wrongly as a request) and makes the understanding of code easier.

If one of your methods is reported as unused, most probably that you did not use the method terminator corresponding to its nature. As such, it is identified as a plain function and is not auto-exported, and thus may be reported as unused.


- destructor: a destruct/1 function shall be available in all cases; if the user did not define it, a default do-nothing implementation will be automatically added (and exported)
- canonical order to enforce: request/oneway/static
- all methods must return their values thanks to a WOOPER construct (i.e. one of these functions associated to the `wooper` module, i.e. ``return_state_result:2``, ``return_state_only/1`` or ``return_static/1``), possibly directly or not (as a method may of course return directly the result of any function), provided that the nested calls remain module-local ; so a method must not terminate on a remote call that is not one of the aforementioned WOOPER constructs (which is not a problematic constraint ; for example, instead of having for last expression ``my_module:my_function(A,B)``, a request shall end with something like ``R=my_module:some_function(A,B), wooper:return_state_result(FinalState,R)``)


 Advantages of meta-programming (thanks to parse-transforms here):
   - no more macro-related corner cases (ex: new() vs new( ?wooper_construct_parameters ))
   - additions (ex: remote_synchronisable_new/N was not defined) can be more easily done
   - some elements (ex: declaration of -spec with the right arities) can be managed whereas they used not to be


If some of your methods are reported as unused (ex: ``function getName/1 is unused``), knowing that methods are automatically exported if needed, it may be the sign that at least one of their clause is not using a WOOPER method terminator.

For example::

 -spec getName( wooper:state() ) -> request_return( name() ).
 getName( State ) ->
	nested_in_request( State ).

 % (helper)
 nested_in_request( State ) ->
	wooper:return_state_result( State, ?getAttr(name) ).


is incorrect, as ``getName/1`` would be detected as a function, whereas ``nested_in_request/1`` would be detected as a request.

This example shall be instead::

 -spec getName( wooper:state() ) -> request_return( name() ).
 getName( State ) ->
	Res = nested_in_request( State ).
	wooper:return_state_result( State, Res ).

 % (helper)
 nested_in_request( State ) ->
	?getAttr(name).

(resulting then in having ``getName/1`` being identified as a request - and thus being auto-exported, and ``nested_in_request/1`` as a function)
