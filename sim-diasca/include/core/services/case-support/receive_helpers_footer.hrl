% Copyright (C) 2012-2021 EDF R&D
%
% This file is part of Sim-Diasca.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
%
% Creation date: July 1, 2007.


% Introduced so that for example static methods performing receives can still
% filter the incoming messages.



% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( case_receive, case_receive() ).



% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( case_receive( AnyMessage ), case_receive( AnyMessage ) ).


% For test support:
-define( test_receive, case_receive() ).
-define( test_receive( AnyMessage ), case_receive( AnyMessage ) ).


% Helper function to write receive clauses in cases which cannot interfere with
% trace supervision, as a case may also receive trace control message the case
% code should be unware of.
%
% Returns the received value.
%
% Ex: Pid ! { getBaz, [], self() }, MyBaz = case_receive(), ...
%
% to be used instead of:
%
% Pid ! { getBaz, [], self() },
% receive
%
%   { wooper_result, V } ->
%			V
%
% end,
% ...
%
% Note that, should the result message be appropriately taggued (hence removing
% any possibility of not selecting the right answer), a standard receive clause
% may be used, however case_receive/1 may still be more appropriate.
%
-spec case_receive() -> any().
case_receive() ->
	traces:receive_applicative_message().



% Helper function to write receive clauses for specific messages in cases while
% not interfering with trace supervision.
%
-spec case_receive( any() ) -> void().
case_receive( Message ) ->
	traces:receive_applicative_message( Message ).



% Test support:
-spec test_receive() -> any().
test_receive() ->
	case_receive().


-spec test_receive( any() ) -> void().
test_receive( Message ) ->
	case_receive( Message ).
