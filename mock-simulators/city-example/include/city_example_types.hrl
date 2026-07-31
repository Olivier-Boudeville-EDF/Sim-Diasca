% Copyright (C) 2012-2026 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2012.


% List of common types defined for The City example.


% Classifying is not straightforward; for example biogas can be a GHG, a
% pollutant and an energy source (fuel).


-doc "Green-house gases (GHG).".
-type green_house_gas() :: 'carbon_dioxide' | 'methane'.


-doc "Gases (all kinds of).".
-type gas() :: green_house_gas() | 'biogas' | 'exhaust_gas' .


-doc "Wastes that could be valorised.".
-type recyclable_waste() :: 'recyclable_waste'.


-doc "Wastes that can be incinerated.".
-type incinerable_waste() :: 'incinerable_waste_type_1'
                           | 'incinerable_waste_type_2'.


-doc "Wastes that cannot be valorised.".
-type treated_waste() :: 'bottom_ash' | 'residual_waste'.


-doc "Quantities of wastes (all kinds of solid wastes, in tons).".
-type waste_type() :: recyclable_waste()
                    | incinerable_waste()
                    | treated_waste().


-doc "Energy used or released, under different possible forms.".
-type energy_form() :: 'heat' | 'electricity' | 'momentum' | 'fuel'.


-doc "Emissions, whether or not they are deemed being pollutants.".
-type emission_type() :: gas() | waste_type() | energy_form().



-type physical_state() :: 'solid' | 'liquid' | 'gaseous'.


-type emission_volume() :: unit_utils:cubic_meters().


-type emission_mass() :: unit_utils:tons().



% Describes an emission.
-record( emission_description, { type   :: emission_type(),
                                 state  :: physical_state(),
                                 volume :: unit_utils:cubic_meters(),
                                 mass   :: unit_utils:tons() } ).


-doc "Describes an emission.".
-type emission_description() :: #emission_description{}.



% Describes a quantity of waste.
-record( waste_description, { type   :: waste_type(),
                              state  :: physical_state(),
                              volume :: unit_utils:cubic_meters(),
                              mass   :: unit_utils:tons() } ).


-doc "Describes a quantity of waste.".
-type waste_description() :: #waste_description{}.



-type tank_id() :: basic_utils:count().



% Models a waste tank, a place where a certain type of waste can be stored.
-record( waste_tank, {

    % To identify this tank in its container (non zero):
    id :: tank_id(),

    % A list of the types of allowed wastes (a type cannot be mixed with
    % another into the same tank):
    allowed_types :: [ waste_type() ],

    % The type of the currently stored waste (if any):
    current_type :: waste_type() | 'none',

    % The current volume of waste stored:
    current_volume_stored :: unit_utils:cubic_meters(),

    % The maximum volume of waste stored:
    max_volume_stored :: unit_utils:cubic_meters(),

    % The current mass of waste stored:
    current_mass_stored :: unit_utils:tons(),

    % The maximum mass of waste stored:
    max_mass_stored :: unit_utils:tons(),

    % Tells whether the tank is being processed (used) or idle:
    busy :: boolean() } ).


-doc """
Models a waste tank, a place where a certain type of waste can be stored.
""".
-type waste_tank() :: #waste_tank{}.


-doc "Describes the waste capacity of a waste storage.".
-type waste_capacity() :: [ waste_tank() ].


-doc "Possible answer to an entry request.".
-type entry_outcome() :: 'entered' | 'entry_refused'.


-doc "All the possible types for a POI.".
-type poi_type() ::  'class_IndustrialWasteSource'
                   | 'class_ResidentialWasteSource'
                   | 'class_Incinerator'
                   | 'class_Landfill'
                   | 'class_RoadJunction'.
