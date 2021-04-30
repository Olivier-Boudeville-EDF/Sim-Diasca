/**
 * Example of a dataflow processing unit that is implemented in Java.
 *
 * @see also its Python counterpart, in vehicle_type_unit.py.
 *
 * @author Julien Guillemineau - julien-externe.guillemineau@edf.fr
 *
 */
package Dataflow_urban_example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangRangeException;

import sim_diasca.DataflowTypes;
import sim_diasca.DataflowTypes.ActivationPolicy;
import sim_diasca.DataflowTypes.ChannelValue;
import sim_diasca.DataflowTypes.InputPortsSpecification;
import sim_diasca.DataflowTypes.OutputPortsSpecification;
import sim_diasca.DataflowTypes.PortConstraints;
import sim_diasca.ProcessingUnit;


public class VehicleTypeUnit extends ProcessingUnit{

	private double aging_factor = 0.03;
	private int year_of_origin;
	private double energy_efficiency;
	private double pollution_ref_efficiency;


	// Unit constructor.
	public VehicleTypeUnit(String block_name, int year_of_origin,
	  double energy_efficiency,
	  double pollution_ref_efficiency) throws OtpErlangRangeException {

		// All input ports are needed in order to perform the computations:
		super(block_name, ActivationPolicy.ACTIVATE_WHEN_ALL_SET,
		  getInputPortSpecification(), getOutputPortSpecification());

		// Sets the attributes specific to this unit (here, type of vehicle):
		this.year_of_origin = year_of_origin;
		this.energy_efficiency = energy_efficiency;
		this.pollution_ref_efficiency = pollution_ref_efficiency;
	}

	@Override
	/**
	 * Method implementing the behaviour of 'vehicle type' units once activated.
	 *
	 * Describes the processing performed by this unit whenever it gets
	 * activated, i.e. when the conditions of its activation policy are met.
	 *
	 * Based on the values read from the unit's input ports, this method
	 * performs the actual, domain-specific computations and sets accordingly
	 * output ports.
	 *
	 */
	public void activate() throws OtpErlangRangeException {

		// Gets the age (in years) of the vehicle type:
		int vehicle_age = this.getSimulation_date_year() - this.year_of_origin;

		this.sendInfo(String.format( "Evaluating the effects of a vehicle type "
			"on the energy consumptions and the pollution emissions for %s.",
			this.getName()));

		// Aggregates all the estimated energy demands (from port iteration):
		String[] energy_estimates = this.getAllInputIterationValues(
		  "energy_demand_estimates");

		// Ditto for all the estimated pollution emissions (from port iteration)
		String[] pollution_estimates = this.getAllInputIterationValues(
		  "pollution_estimates");

		/* Computes the actual impacts of the vehicle(s) in terms of energy
		 * demand and pollution (domain-specific core of the model)
		 */
		double[] actual_impact = this.computeVehicleEffects(energy_estimates,
		  pollution_estimates, vehicle_age);

		/* Sets the computed values, along with relevant meta data, for
		 * assignment to the output ports.
		 */
		ChannelValue energy_output = this.createChannelValue(
		  actual_impact[0],
		  "http://foobar.org/urban/1.1/energy/demand",
		  "kW.h", "double");

		ChannelValue pollution_output = this.createChannelValue(
		  actual_impact[1],
		  "http://foobar.org/urban/1.1/pollution/emission",
		  "g.cm^-3", "double");

		Map<String, ChannelValue> map = new HashMap<>();
		map.put("actual_energy_need", energy_output);
		map.put("actual_pollution", pollution_output);
		this.setOutputPortValues(map);

	}

	/**
	 * This is the core of this "vehicle efficiency" pseudo-model, the function
	 * where its actual domain-specific computations are performed from the
	 * values received from dataflow.
	 *
	 * Note: this logic is pure, has strictly no link with anything related to a
	 * dataflow or even to the internal state of this unit.
	 *
	 * @param energy_estimates
	 * @param pollution_estimates
	 * @param vehicle_age
	 *
	 */
	private double[] computeVehicleEffects(String[] energy_estimates,
	  String[] pollution_estimates, int vehicle_age) {

		/* Effect 1: energy efficiency (simple rule, using a flat, constant
		 * coefficient).
		 */

		long energy_l = 0;
		for (String energy : energy_estimates) {
			energy_l += Long.parseLong(energy);
		}
		double actual_energy_demand = energy_l * vehicle_age;

		/*
		 * Effect 2: pollution emission (rule using a coefficient increasing
		 * with the age of the vehicle)
		 */
		double pollution_coeff = 1 + this.aging_factor * vehicle_age;
		long pollution_l = 0;
		for (String pollution : pollution_estimates) {
			pollution_l += Long.parseLong(pollution);
		}
		double actual_pollution =
			pollution_l * this.pollution_ref_efficiency * pollution_coeff;

		this.sendDebug(String.format(
			"Impacts of the vehicle(s) of type %s: energy needed is %s kW.h, "
			+ "pollution emitted is %s g.cm^-3", this.getName(),
			actual_energy_demand,actual_pollution));

		return new double[] {actual_energy_demand,actual_pollution};

	}

	/**
	 * Returns a textual description of this vehicle unit.
	 *
	 */
	public String getUnitDescription() {

		return String.format("Processing unit modeling the %s vehicle type, "
		  "introduced in %s (aged of %s years). "
		  + "Its original characteristics are an efficiency coefficient "
		  "of %s in terms of energy consumption and another of %s "
		  "in terms of pollution emitted.",
		  this.getName(), this.year_of_origin,
		  this.getSimulation_date_year() - this.year_of_origin,
		  this.energy_efficiency, this.pollution_ref_efficiency );

	}

	/**
	 * Statically defines the specifications of the input and output ports of
	 * these processing units.
	 *
	 */
	public static List<List<?>> getPortSpecifications() {
		List<List<?>> port_specs = new ArrayList<List<?>>();
		port_specs.add(VehicleTypeUnit.getInputPortSpecification());
		port_specs.add(VehicleTypeUnit.getOutputPortSpecification());

		return port_specs;
	}

	/**
	 * Defines the input port specifications of these units.
	 *
	 */
	public static List<InputPortsSpecification> getInputPortSpecification(){

		dataflow = new DataflowTypes();

		InputPortsSpecification energy = dataflow.new InputPortsSpecification(
		  "energy_demand_estimates",
		  "Each of these iterated ports tracks a source of (estimated) "
		  "energy demand",
		  true,
		  "http://foobar.org/urban/1.1/energy/demand",
		  "kW.h",
		  "double",
		  new ArrayList<>(Arrays.asList(PortConstraints.POSITIVE)));

		InputPortsSpecification pollution =
			dataflow.new InputPortsSpecification(
			  "pollution_estimates",
			  "Each of these iterated ports tracks a source of "
			  "(estimated) pollution",
			  true,
			  "http://foobar.org/urban/1.1/pollution/emission",
			  "g.cm^-3",
			  "double",
			  new ArrayList<>(Arrays.asList(PortConstraints.POSITIVE)));

		List<InputPortsSpecification> ports_specifications =
			Arrays.asList(energy,pollution);

		return ports_specifications;

	}

	/**
	 * Defines the output port specifications of these units.
	 *
	 */
	public static List<OutputPortsSpecification> getOutputPortSpecification(){

		dataflow = new DataflowTypes();

		OutputPortsSpecification energy = dataflow.new OutputPortsSpecification(
		  "actual_energy_need",
		  "Aggregated energy need of households sharing "
		  "the same vehicle type",
		  "http://foobar.org/urban/1.1/energy/demand",
		  "kW.h",
		  "double",
		  new ArrayList<>(Arrays.asList(PortConstraints.POSITIVE)));

		OutputPortsSpecification pollution =
			dataflow.new OutputPortsSpecification(
			  "actual_pollution",
			  "Aggregated pollution emitted by households sharing "
			  "the same vehicle type",
			  "http://foobar.org/urban/1.1/pollution/emission",
			  "g.cm^-3",
			  "double",
			  new ArrayList<>(Arrays.asList(PortConstraints.POSITIVE)));

		List<OutputPortsSpecification> ports_specifications = Arrays.asList(
		  energy, pollution);

		return ports_specifications;
	}

	/**
	 * Statically declares the only semantics that should be handled by the
	 * ports of the instances of this unit.
	 *
	 */
	public static List<String> getDeclaredSemantics() {
		List<String> semantics = new ArrayList<String>();
		semantics.add("http://foobar.org/urban/1.1/energy/demand");
		semantics.add("http://foobar.org/urban/1.1/pollution/emission");
		return semantics;
	}
}
