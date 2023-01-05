% Copyright (C) 2016-2023 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]


% Note:
%
% For these simulations, we consider that external_id() is
% text_utils:bin_string().


-include("dataflow_defines.hrl").


-type urban_dataflow_event() :: 'new_energy_demand_unit_needed'.
% The dataflow-level events that unit managers may handle.


-type event_data() :: any().
% Event-associated data.


% Centralisation of defines, type definitions, semantics, etc. for the Urban
% Example.



% Processing units:

-type transport_unit_pid() :: unit_pid().

-type energy_unit_pid() :: unit_pid().

-type energy_demand_unit_pid() :: unit_pid().

-type vehicle_unit_pid() :: unit_pid().



% Dataflow objects:

-type district_pid() :: object_pid().

-type building_pid() :: object_pid().

-type household_pid() :: object_pid().



% Here we define a few types mirroring the ones handled by dataflow ports, for
% convenience when writing the code operating on their actual values (these
% types are not used at all by anything related to the dataflow):

-type adult_count() :: basic_utils:count().

-type child_count() :: basic_utils:count().

-type average_journey() :: float().

-type area_type() :: 'rural' | 'urban'.

-type energy_demand() :: float().

-type pollution_level() :: float().

-type transformer_efficiency() :: float().


% Case-specific glossary (centralisation of the definitions of semantics).

-define( adult_count_semantics,
		 "http://foobar.org/urban/1.1/population/count/adults" ).

-define( child_count_semantics,
		 %"http://foobar.org/urban/1.1/population/count/children" ).

		 % To test whether two semantics almost identically named are rejected
		 % (now: not anymore)
		 "http://foobar.org/urban/1.1/population/count/adult" ).

-define( average_population_gain,
		 "http://foobar.org/urban/1.1/population/average_gain" ).

-define( average_savings,
		 "http://foobar.org/urban/1.1/finance/average_savings" ).

-define( area_type_semantics,
		 "http://foobar.org/urban/1.1/area/type" ).

-define( path_length_semantics,
		 "http://foobar.org/urban/1.1/path/length" ).

-define( energy_demand_semantics,
		 "http://foobar.org/urban/1.1/energy/demand" ).

-define( pollution_emission_semantics,
		 "http://foobar.org/urban/1.1/pollution/emission" ).

-define( transformation_efficiency_semantics,
		 "http://foobar.org/urban/1.1/transformation/efficiency" ).

-define( address_semantics, "http://foobar.org/urban/1.1/postal_address" ).

-define( name_semantics, "http://foobar.org/urban/1.1/name" ).

-define( income_semantics, "http://foobar.org/urban/1.1/income" ).

-define( surface_semantics, "http://foobar.org/urban/1.1/surface" ).

-define( floor_count_semantics, "http://foobar.org/urban/1.1/floor_count" ).


% Introduced to check that values bearing an extra, unrelated semantics are not
% rejected by ports:
%
-define( extra_semantics, "http://foobar.org/extra/unrelated" ).
