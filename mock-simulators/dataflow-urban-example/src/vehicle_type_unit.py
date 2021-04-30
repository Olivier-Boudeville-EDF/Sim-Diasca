# Copyright (C) 2016-2017 EDF R&D
#
# This file is part of Sim-Diasca.
#
# Sim-Diasca is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# Sim-Diasca is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Sim-Diasca.
# If not, see <http://www.gnu.org/licenses/>.

# Author: Robin Huart (robin-externe.huart@edf.fr)


# Currently, workbench-related developments rely on a Python virtual
# environment, whereas the engine itself directly imports modules.

try:
    # If run directly from the engine:
    from sim_diasca.dataflow_types import *
    from sim_diasca.processing_unit import ProcessingUnit

except ImportError:
    # If run from a Python virtual environment:
    from .dataflow_types import *
    from .processing_unit import ProcessingUnit


# ProcessingUnit is the abstract mother class from which all (Python) processing
# units must inherit.


# This unit is known from the coupling architecture as class_VehicleTypeUnit.
# As such, the corresponding (Python) class is expected to be named
# VehicleTypeUnit and be implemented in the vehicle_type_unit module.

# ProcessingUnit inherits from Block, which defines following trace-emitting
# methods (of increasing severity): debug, trace, info, warning, error, fatal.


# Design notes:
#
# The overall vehicle model is based on a yearly timestep.
# This unit relies on port iterations (no plain port used here).


# Implementation notes:
#
# ActivationPolicy is an enumeration.


# Definition of the VehicleTypeUnit processing unit in Python.
#
# Example of a dataflow unit evaluating, in the context of the 'Dataflow Urban
# Example' case, the impact of the choice of a particular vehicle made by some
# households on their overall energy demand and pollution emissions.
#
# Instead of multiplying the units (ex: one per vehicle instance), whenever two
# households own the same vehicle (i.e. have the same vehicle type), we
# aggregate their consumptions and emissions inside the same unit in charge of
# that vehicle type (thanks to port iterations), and then we compute the
# impact on these aggregated data made by the vehicle (depending on its
# characteristics).
#
class VehicleTypeUnit(ProcessingUnit):
    """
    Example of dataflow processing unit in terms of energy demand and emitted
    pollution for a given vehicle type, in the context of the **Dataflow Urban
    Example** case.
    """

    #
    # Contructor method for a 'vehicle type' processing unit:
    #
    def __init__(self, name: BlockName, year_of_origin: int,
                 energy_efficiency: float, pollution_ref_efficiency: float):
        """
        Initialisation of this processing unit.
        """

        # All input ports are needed in order to perform the computations:
        policy = ActivationPolicy.ACTIVATE_WHEN_ALL_SET

        input_ports_specs, output_ports_specs = self.get_port_specifications()

        super().__init__(name, policy, input_ports_specs, output_ports_specs)

        # Sets the attributes specific to this unit (here, type of vehicle):
        self.aging_factor = 0.03
        self.year_of_origin = year_of_origin
        self.energy_efficiency = energy_efficiency
        self.pollution_ref_efficiency = pollution_ref_efficiency

    #
    # Method implementing the behaviour of 'vehicle type' units once activated:
    #
    def activate(self) -> None:
        """
        Describes the processing performed by this unit whenever it gets
        activated, i.e. when the conditions of its activation policy are met.

        Based on the values read from the unit's input ports, this method
        performs the actual, domain-specific computations and sets accordingly
        output ports.
        """

        print("[Python-executed] Vehicle unit activated!")

        # Gets the age (in years) of the vehicle type:
        vehicle_age = self.get_simulation_date()[0][0] - self.year_of_origin

        self.send_info("Evaluating the effects of a vehicle type on the "
                       "energy consumptions and the pollution emissions for "
                       "{}.".format(self.get_block_name()))

        # Aggregates all the estimated energy demands (from port iteration):
        energy_estimates = self.get_all_input_iteration_values(
            'energy_demand_estimates')

        # Ditto for all the estimated pollution emissions (from port
        # iteration):
        pollution_estimates = self.get_all_input_iteration_values(
            'pollution_estimates')

        # Computes the actual impacts of the vehicle(s) in terms of energy
        # demand and pollution (domain-specific core of the model):
        actual_energy_demand, actual_pollution_emission = \
            self.__compute_vehicle_effects(energy_estimates,
                                           pollution_estimates, vehicle_age)

        # Sets the computed values, along with relevant metadata, for assignment
        # to the output ports:
        energy_output = self.create_channel_value(actual_energy_demand,
                                                  ['http://foobar.org/urban/1.1/energy/demand'],
                                                  "kW.h", "float")

        pollution_output = self.create_channel_value(actual_pollution_emission,
                                                     ['http://foobar.org/urban/1.1/pollution/emission'],
                                                     "g.cm^-3", "float")

        self.set_output_port_values([('actual_energy_need', energy_output),
                                     ('actual_pollution', pollution_output)
                                     ])

    #
    # This is the core of this "vehicle efficiency" pseudo-model, the function
    # where its actual domain-specific computations are performed from the
    # values received from dataflow.
    #
    # Note: this logic is pure, has strictly no link with anything related to a
    # dataflow or even to the internal state of this unit.
    #
    # (private member method)
    #
    def __compute_vehicle_effects(self, energy_estimates, pollution_estimates,
                                  vehicle_age):
        """
        Computes the modifications on energy consumption and pollution
        emissions due to the usage of a vehicle, from its characteristics.
        """

        # Effect 1: energy efficiency
        # (simple rule, using a flat, constant coefficient)
        actual_energy_demand = sum(energy_estimates) * self.energy_efficiency

        # Effect 2: pollution emission
        # (rule using a coefficient increasing with the age of the vehicle)
        pollution_coeff = 1 + self.aging_factor * vehicle_age
        actual_pollution = sum(pollution_estimates) * \
            self.pollution_ref_efficiency * pollution_coeff

        # Informational trace message:
        self.send_debug("Impacts of the vehicle(s) of type {}: energy needed "
                        "is {} kW.h, pollution emitted is {} g.cm^-3".format(
                            self.get_block_name(), actual_energy_demand,
                            actual_pollution))

        return (actual_energy_demand, actual_pollution)

    #
    # Textual description of a 'vehicle type' object:
    #
    def get_unit_description(self) -> str:
        """
        Returns a textual description of this vehicle unit.
        """

        return "Processing unit modeling the {} vehicle type, introduced in " \
               "{} (aged of {} years). Its original characteristics are "     \
               "an efficiency coefficient of {} in terms of energy " \
               "consumption and another of {} in terms of pollution emitted." \
               .format(self.get_block_name(), self.year_of_origin,
                       self.get_simulation_date()[0][0] - self.year_of_origin,
                       self.energy_efficiency, self.pollution_ref_efficiency)

    #
    # Static section
    #

    @staticmethod
    def get_port_specifications():
        """
        Statically defines the specifications of the input and output ports of
        the *vehicle_type_unit* processing units.
        """
        return (VehicleTypeUnit.get_input_port_specifications(),
                VehicleTypeUnit.get_output_port_specifications())

    @staticmethod
    def get_input_port_specifications() -> list:
        """
        Statically defines the input port specifications.
        """

        energy_input_ports_spec = InputPortSpecification(
            name='energy_demand_estimates',
            comment="Each of these iterated ports tracks a source of "
            "(estimated) energy demand",
            is_iteration=True,
            value_semantics=['http://foobar.org/urban/1.1/energy/demand'],
            value_unit="kW.h",
            value_type_description='float',
            value_constraints=[PortConstraints.POSITIVE])

        pollution_input_ports_spec = InputPortSpecification(
            name='pollution_estimates',
            comment="Each of these iterated ports tracks a source of "
            "(estimated) pollution",
            is_iteration=True,
            value_semantics=[
                'http://foobar.org/urban/1.1/pollution/emission'],
            value_unit="g.cm^-3",
            value_type_description='float',
            value_constraints=[PortConstraints.POSITIVE])

        return [energy_input_ports_spec, pollution_input_ports_spec]

    @staticmethod
    def get_output_port_specifications() -> list:
        """
        Statically defines the output port specifications of that unit.
        """

        energy_output_port_spec = OutputPortSpecification(
            name='actual_energy_need',
            comment="Aggregated energy need of households sharing the "
            "same vehicle type",
            value_semantics=['http://foobar.org/urban/1.1/energy/demand'],
            value_unit="kW.h",
            value_type_description='float',
            value_constraints=[PortConstraints.POSITIVE])

        pollution_output_port_spec = OutputPortSpecification(
            name='actual_pollution',
            comment="Aggregated pollution emitted by households sharing "
            "the same vehicle type",
            value_semantics=[
                'http://foobar.org/urban/1.1/pollution/emission'],
            value_unit="g.cm^-3",
            value_type_description='float',
            value_constraints=[PortConstraints.POSITIVE])

        return [energy_output_port_spec, pollution_output_port_spec]

    # Returns the semantics statically declared by this unit:
    #
    # Defining this method allows to ensure that all the ports ever created by
    # this unit will use the semantics among this explicitly stated list.
    #
    # Otherwise the list would be deduced from the initial port specifications,
    # with no specific control.
    #
    # (optional)
    #
    @staticmethod
    def get_declared_semantics():
        """
        Statically declares the only semantics that should be handled by the
        ports of the instances of this unit.
        """
        return ['http://foobar.org/urban/1.1/energy/demand',
                'http://foobar.org/urban/1.1/pollution/emission']

    # Returns the types statically declared by this unit:
    #
    # (optional)
    #
    @staticmethod
    def get_declared_types():
        """
        Statically declares the only types that should be handled by the ports
        of the instances of the VehicleTypeUnit class.
        """

        # No specific type to declare here, as we rely only on built-in ones
        # (floats):
        return []
