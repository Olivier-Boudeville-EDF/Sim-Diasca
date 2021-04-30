:raw-latex:`\pagebreak`


.. _units:

Management of Units
===================



Motivation
----------

A value of a given type (ex: a float) can actually correspond to quantities as different as meters and kilowatts per hour.

Therefore **units shall preferably be specified alongside with values being processed**, and a language to express, check and convert these units must be retained. Of course units are of interest as other metadata are - such as accuracy, semantics, etc.



Available Support
-----------------

The ``Myriad`` layer provides such a service, in a very basic, ad hoc form (which is useful to introduce "special" non-physical, non-standard units, such as ``euro/year``), meant to be enriched over time.



Specifying Units
----------------


Aliases
.......

For convenience, *aliases* of units can be defined, i.e. alternate names for a given canonical unit. For example the Hertz unit (``Hz``) is an alias of the ``s^-1`` (per-second) canonical unit.


Built-in Units
..............

So one may use the following **built-in units**, whose symbol [#]_ is specified here between brackets, like in ``"[N.m]"`` (an alternate notation is to prefix a unit with ``U:``, like in ``"U: N.m"``):

- the seven `SI base units <https://en.wikipedia.org/wiki/SI_base_unit>`_, namely:

 - meter, for length [``m``]
 - gram, for mass [``g``] [#]_ (note: this is a footnote, not an exponent!)
 - second, for time [``s``]
 - ampere, for electric current [``A``]
 - kelvin, for thermodynamic temperature [``K``]
 - mole, for the amount of substance [``mol``]
 - candela, for luminous intensity [``cd``]

- the usual `derived units <https://en.wikipedia.org/wiki/SI_derived_unit>`_, notably:

 - hertz, for frequency [``Hz``]
 - degree, for degree of arc [``°``] (not supported yet)
 - radian, for angle [``rad``] (not supported yet)
 - steradian, for solid angle [``sr``] (not supported yet)
 - newton, for force, weight [``N``]
 - pascal, for pressure, stress [``Pa``]
 - joule, for energy, work, heat [``J``]
 - watt, for power, radiant flux [``W``]
 - coulomb, for electric charge, quantity of electricity [``C``]
 - volt, for voltage, electrical potential difference, electromotive force [``V``]
 - farad, for electrical capacitance [``F``]
 - ohm, for electrical resistance, impedance, reactance [``Ohm``]
 - siemens, for electrical conductance [``S``]
 - weber, for magnetic flux [``Wb``]
 - tesla, for magnetic field strength, magnetic flux density [``T``]
 - henry, for inductance [``H``]
 - lumen, for luminous flux [``lm``]
 - lux, for illuminance [``lx``]
 - becquerel, for radioactive decays per unit time [``Bq``]
 - gray, for absorbed dose of ionizing radiation [``Gy``]
 - sievert, for equivalent dose of ionizing radiation [``Sv``]
 - katal, for catalytic activity [``kat``]

- the units **widely used** in conjunction with SI units (note that they may not respect the principle of being a product of integer powers of one or more of the base units):

 - litre, for 10\ :superscript:`-3`\ m\ :superscript:`3`\ volumes [``L``]
 - tonne, for 1,000 kilogram masses [``t``]
 - electronvolt, for 1.602176565(35).10-19 joule energies  [``eV``]
 - minute, for 60-second durations [``min``]
 - hour, for 60-minute durations [``h``]
 - day, for 24-hour durations [``day``]
 - week, for 7-day durations [``week``]


- the **special** units (they generally cannot map directly to any SI unit, yet can be handled separately), designating:

 - month [``month``] (correspondence to base time units unspecified, as this duration is not constant; ex: a month can be 29, 30 or 31 days)
 - year [``year``] (correspondence to base time units unspecified, as this duration is not constant; ex: a year can be 365, 366 or 365.25 days, etc.)
 - degree Celsius, for temperature relative to 273.15 K [``°C``] (see note below)
 - dimension-less quantities (ex: an index) [``dimensionless``] (most probably clearer than ``m/m``)
 - a count, i.e. a dimensionless number, generally a positive integer [``count``] (ex: ``14``), considered as an alias of ``dimensionless``
 - a ratio, i.e. a dimensionless floating-point value, generally displayed as a percentage [``ratio``] (ex: ``-12.9%``); another alias of ``dimensionless``
 - currencies, either [``$``] (US Dollar) or [``euros``] (Euro), whose exchange rates of course vary
 - values whose unit has not been specified [``unspecified_unit``]


- `metric prefixes <https://en.wikipedia.org/wiki/Metric_prefix>`_ thereof, i.e. multiples and sub-multiples of the units previously mentioned; currently the supported prefixes are:

 - yotta, i.e. 10\ :superscript:`24`\  [``Y``]
 - zetta, i.e. 10\ :superscript:`21`\  [``Z``]
 - exa, i.e. 10\ :superscript:`18`\  [``E``]
 - peta, i.e. 10\ :superscript:`15`\  [``P``]
 - tera, i.e. 10\ :superscript:`12`\  [``T``]
 - giga, i.e. 10\ :superscript:`9`\  [``G``]
 - mega, i.e. 10\ :superscript:`6`\  [``M``]
 - kilo, i.e. 10\ :superscript:`3`\  [``k``]
 - hecto, i.e. 10\ :superscript:`2`\  [``h``]
 - deca, i.e. 10 [``da``]
 - deci, i.e. 10\ :superscript:`-1`\  [``d``]
 - centi, i.e. 10\ :superscript:`-2`\  [``c``]
 - milli, i.e. 10\ :superscript:`-3`\  [``m``]
 - micro, i.e. 10\ :superscript:`-6`\  [``µ``]
 - nano, i.e. 10\ :superscript:`-9`\  [``n``]
 - pico, i.e. 10\ :superscript:`-12`\  [``p``]
 - femto, i.e. 10\ :superscript:`-15`\  [``f``]
 - atto, i.e. 10\ :superscript:`-18`\  [``a``]
 - zepto, i.e. 10\ :superscript:`-21`\  [``z``]
 - yocto, i.e. 10\ :superscript:`-24`\  [``y``]


 .. [#] To avoid requesting the user to type specific Unicode characters, we transliterated some of the symbols. For example, instead of using the capital Omega letter, we used ``Ohm``.

 .. [#] We preferred here deviating a bit from the SI system, by using this non-prefixed unit (the *gram*) instead of the SI standard one, which happens to be the *kilogram*.


.. Note::

   There is a problem with temperatures, as they can be expressed at least in kelvins or degrees Celsius, whereas the two corresponding scales do not match, since there is an offset:

	 [K] = [°C] + 273.15

   As a result, unit conversions would require updating as well the corresponding value, and, more generally, they should be treated as fully distinct units (ex: ``kW/°C`` cannot be automatically converted in terms of SI base units, i.e. using ``K``).

   This is why we "degraded" Celsius degrees, from a derived unit to a special one.

   The same applies to the Fahrenheit unit (a likely addition), as:

	 [°C] = 5/9.([°F]-32)




Composing One's Units
.....................

So an actual unit can be composed from the aforementioned built-in units (be they base, derived, widely used, special units; prefixed or not) [#]_, using two built-in operators, which are ``"."`` (multiply, represented by the dot character - not ``"*"``) and ``"/"`` (divide, represented by the forward slash character).

.. [#] In the future, defining an actual unit from other actual units might be contemplated.

The resulting type shall be specified as a string, containing a series of built-in units (potentially prefixed) alternating with built-in operators, like in: ``"kW.s/m"``.

.. Note::
   As a result, ``"kWh"`` is not a valid unit: it should be denoted as ``"kW.h"``.

   Similarly, ``"W/(m.k)"`` is not valid, since parentheses are currently not supported: ``"W/m/k"`` may be used instead.



Finally, exponents can be used as a shorthand for both operators (ex: ``kg.m^2.s^-1``, instead of ``kg.m.m/s``). They should be specified explicitly, thanks to the caret character (``"^"``); for example ``"m^2/s"``, not ``"m²/s"``.


If deemed both safe and useful, we may consider in the future performing:

- symbolic unit checking (i.e. determining that a derived unit such as ``N.s`` (newton.second) is actually, in canonical SI units, ``m^2.kg.s^-1``), and thus that values of these two types can safely be used indifferently in computations

- automatic value conversions (ex: converting ``km/hour`` into ``m/s``), provided that the overall computational precision is not significantly deteriorated


The corresponding mechanisms (type information, conversion functions, unit checking and transformation, etc.) are defined in ``unit_utils.erl`` and tested in ``unit_utils_test.erl``,  in the ``myriad/src/utils`` directory.



Checking Units
..............

A typical example:

.. code:: erlang

 1> MyInputValue="-24 mS.m^-1".
 2> {Value,Unit}=unit_utils:parse_value_with_unit(MyInputValue).
 3> io:format("Corresponding value: ~f.~n", [ Value ] ).
 Corresponding value: -24.0.
 4> io:format("Corresponding unit: ~s.~n",
	[unit_utils:unit_to_string(Unit)]).
 "s^3.A^2.g^-1.m^-3, of order -6"
 5> unit_utils:value_with_unit_to_string(Value,Unit).
 "-2.4e-5 s^3.A^2.g^-1.m^-3"



Possible Improvements Regarding Dimensional Analysis
----------------------------------------------------

Some programming languages provide systems to manage dimensional information (ex: for physical quantities), generally through add-ons or libraries (rarely as a built-in feature).

A first level of support is to provide, like here, an API to manage units. Other levels can be:

1. to integrate unit management directly, seamlessly in language expressions, as if it was built-in (as opposed to having to use explicitly a third-party API for that); for example at least half a dozen different libraries provide that in Python
2. to be able to define "polymorphic units and functions", for example to declare in general that a speed is a distance divided by a duration, regardless of the possible units used for that
3. to perfom *static* dimensional analysis, instead of checking units at runtime

The two latter use cases can for example be at least partially covered by Haskell libraries.
