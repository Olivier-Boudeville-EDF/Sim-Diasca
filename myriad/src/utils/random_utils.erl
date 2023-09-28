% Copyright (C) 2007-2023 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: July 1, 2007.


% @doc Gathering of various <b>random-related</b> facilities, based on
% probability distributions, either as Myriad built-in ones (uniform,
% exponential, gaussian, etc.) or user-defined, arbitrary ones.
%
% See random_utils_test.erl for the corresponding test.
%
-module(random_utils).



% Usage notes.

% The user specifies first a random law (see random_law_spec/0), from which the
% actual random law can be initialised (built, "compiled") so that any number of
% samples can then be obtained from it.

% Most laws come in their normal, canonical version (e.g. 'exponential_1p'),
% typically yielding floating-point values, and also in one yielding only
% integer values, possibly even only positive ones
% (e.g. 'positive_integer_exponential_1p').

% There were initially three basic classes of built-in random distributions:
% - uniform (a.k.a. white noise)
% - exponential
% - Gaussian (a.k.a. normal)
%
% Then various reliability-related distributions have been added (e.g. the
% Weibull family).
%
% Often distributions are generalised with parameters that allow specialising
% them into well-known distributions (which are thus special cases thereof).
%
% Upcoming built-in additions could be:
% - the Dirac delta distribution (a.k.a the unit impulse)
% - some instantaneous distribution (to be determined)



% Functions for random management.

% As a general rule of thumb, if generating random values in an interval that
% are:
%  - integer: then both bounds are included
%  - floating point: then the lower bound is included and the upper one is
%  excluded

% In this module, "positive integer" must be understood as comprising zero
% (otherwise "strictly positive integer" shall be used).



% Design notes:
%
% Two methods are used to evaluate distribution functions, depending on their
% nature:
%
%  - for a few of them (uniform, and the ones that can be directly derived from
%  it, as exponential and gaussian), a direct sampling can be done, with
%  infinite precision (samples do not have to be gathered in buckets)
%
%  - for the rest of them (the majority; e.g. Weibull, arbitrary PDF, etc.),
%  their PDF has to be discretised; as a result, a fixed, relatively low number
%  of discretisation buckets is used
%
% In the first case (direct sampling), each sample can be seen as being its own
% bucket, and a floating-point sample, being a C double, is very unlikely to be
% drawn more than once; as a result, their probability-like will be constant,
% i.e. the inverse of the number of the drawn samples (and of course normalising
% would not change anything to that); only the density of drawing in an area
% will denote higher PDF values.
%
% Unless performing a colossal number of drawings, distributions that would be
% directly sampled cannot be represented in terms of frequency - unless they get
% discretised as well.
%
% Distributions able to be directly sampled do not require bounds to be
% specified or determined, as opposed to the ones whose PDF has to be
% discretised.



% Service usage, state and seeding:
-export([ start_random_source/3, start_random_source/1, can_be_seeded/0,
		  reset_random_source/1, stop_random_source/0,

		  get_random_module_name/0,
		  get_random_state/0, set_random_state/1,

		  get_random_seed/0, check_random_seed/1 ]).


% Functions related to random laws in general:
-export([ get_law_name/1, get_all_sample_pairs/1,
		  initialise_law/1, get_law_settings/1,
		  %get_sampling_info/1,
		  sampling_info_to_string/1, sampling_info_to_string/2,
		  law_spec_to_string/1, law_data_to_string/1 ]).


% Functions related to uniform sampling:
-export([ get_uniform_value/0, get_uniform_value/1, get_uniform_value/2,
		  get_uniform_values/2, get_uniform_values/3,

		  get_uniform_floating_point_value/1,
		  get_uniform_floating_point_value/2,

		  get_boolean/0, one_of/1, get_random_subset/2 ]).


% Ad hoc functions specific to exponential sampling:
-export([ get_exponential_1p_value/1,

		  get_positive_integer_exponential_1p_value/1,

		  get_exponential_1p_values/2,

		  get_positive_integer_exponential_1p_values/2

		  %get_exponential_2p_value/2,
		  %get_positive_integer_exponential_2p_value/2,
		  %get_exponential_2p_values/3,
		  %get_positive_integer_exponential_2p_values/3

		]).


% Ad hoc functions specific to gaussian sampling:
-export([ get_gaussian_value/2, get_positive_integer_gaussian_value/2,
		  get_gaussian_values/3, get_positive_integer_gaussian_values/3 ]).



% Integrated functions relative to all distributions:
-export([ exponential_1p_pdf/2, exponential_2p_pdf/3,

		  gamma_2p_pdf/3, gamma_3p_pdf/4,

		  gumbel_2p_pdf/3,

		  loglogistic_2p_pdf/3, loglogistic_3p_pdf/4,

		  lognormal_2p_pdf/3, lognormal_3p_pdf/4,

		  gaussian_pdf/3,

		  weibull_2p_pdf/3, weibull_3p_pdf/4,
		  weibull_cr_pdf/4, weibull_ds_pdf/4,
		  weibull_dszi_pdf/5,
		  weibull_mixture_pdf/6,
		  weibull_zi_pdf/4,

		  beta_2p_pdf/3 ]).


% Silencing (unused, as computed directly):
-export([ get_exponential_1p_pdf/2 ]).


% Functions related to arbitrary, non-uniform sampling:
-export([ generate_alias_table_from/1,
		  get_sample_from/1, get_samples_from/2 ]).




-type seed_element() :: integer().
% Not future-proof enough (e.g. not compliant with other solutions like
% SIMD-oriented Fast Mersenne Twister).

-type seed() :: { seed_element(), seed_element(), seed_element() }.
% A type of seed for random generators.


% random:ran/0 does not seem exported, replaced by seed/0:
-type random_state() :: seed()
					  | rand:state()
					  | any().
% For simpler generators, the state is just a seed, for all the others the state
% may be much larger/more complex.
%
% Not to be mixed with the static data (random_law_data/0) corresponding to a
% random law, so that samples can be obtained from it.


-record( alias_table, {

	% Total number of sample entries:
	entry_count :: sample_count(),

	% Array referencing all declared samples:
	sample_values :: array( sample() ),

	% Array keeping track of the sample corresponding to each category:
	indexes :: array( positive_index() ),

	% Array of probability-like for each category:
	prob_likes :: array( probability_like() ) } ).


-opaque alias_table() :: #alias_table{}.
% The static information corresponding to a random law in charge of producing
% samples according to the specified discrete probability distribution, based on
% the alias method.



-type sample( T ) :: T.
% The type of a sample that can be drawn from a probability distribution; a
% probability may be indeed associated to any kind of samples (integer ones,
% strings, vectors, etc.).


-type sample() :: sample( any() ).
% Designates a sample value of unknown type.
%
% It may be a number or a symbol (for example: 'obverse', 'reverse').


-type float_sample() :: sample( float() ).
% Designates a floating-point sample value.

-type positive_float_sample() :: sample( float() ).
% Designates a positive floating-point sample value.


-type sample_count() :: count().
% A number of samples.


-type increment() :: float().
% The increment added between two discretisation steps.


%-type discrete_sampling_info() :: sample_count().
% Information regarding a sampling among discrete (unordered) values.


%-type interval_sampling_info() ::
%	{ StartSample :: float_sample(), StopSample :: float_sample(),
%	  sample_count() }.
% Information regarding a numerical (float-based) sampling on an interval.


%-type sampling_info() :: discrete_sampling_info() | interval_sampling_info().
% Information regarding a (float-based) sampling.


-type sample_entry( T ) :: { sample( T ), probability_like() }.
% An entry corresponding to a sample of the specified type in a probability
% distribution.

-type sample_entry() :: sample_entry( any() ).
% An entry corresponding to a sample of unspecified type in a probability
% distribution.


-type rate() :: number().
% A rate parameter, typically for the Lambda parameter of the exponential law.

-type shape() :: number().
% A shape parameter of a random law.

-type scale() :: number().
% A scale parameter of a random law.


-type mean() :: number().
% An arithmetic mean of a list of numbers, that is the sum of all of the numbers
% divided by the number of numbers.
%
% See https://en.wikipedia.org/wiki/Mean#Arithmetic_mean_(AM)


-type standard_deviation() :: math_utils:standard_deviation().
% A measure of the amount of dispersion of a set of values.
%
% It is the square root of its variance.
%
% See https://en.wikipedia.org/wiki/Standard_deviation


% Section for the description of random laws (probability distributions).


-type law_name() :: atom().
% The name, as an atom, of a type of (unparametrised) law.
%
% For example, 'uniform' or 'weibull_3p'.
%
% Acts as an identifier of this type of laws.




% Subsection for law specifications.
%
% Note that some distributions can be defined with different numbers of
% parameters; for clarity, a distribution named 'foobar' defined based on N
% parameters will be designated as foobar_Np (e.g. lognormal_3p).


-type random_law_spec() ::
	uniform_law_spec() | full_uniform_law_spec()
  | integer_uniform_law_spec()

  | exponential_1p_law_spec() %| full_exponential_1p_law_spec()
  | exponential_law_spec()

  | exponential_2p_law_spec() | full_exponential_2p_law_spec()

  | positive_integer_exponential_1p_law_spec()

  | gamma_2p_law_spec() | full_gamma_2p_law_spec()
  | gamma_3p_law_spec() | full_gamma_3p_law_spec()
  | full_gamma_law_spec()

  | gaussian_law_spec() | normal_2p_law_spec()
  | positive_integer_gaussian_law_spec()

  | gumbel_2p_law_spec() | full_gumbel_2p_law_spec()

  | loglogistic_2p_law_spec() | full_loglogistic_2p_law_spec()
  | loglogistic_3p_law_spec() | full_loglogistic_3p_law_spec()

  | lognormal_2p_law_spec() | full_lognormal_2p_law_spec()
  | lognormal_3p_law_spec() | full_lognormal_3p_law_spec()

  | weibull_2p_law_spec()      | full_weibull_2p_law_spec()
  | weibull_3p_law_spec()      | full_weibull_3p_law_spec()
  | weibull_cr_law_spec()      | full_weibull_cr_law_spec()
  | weibull_ds_law_spec()      | full_weibull_ds_law_spec()
  | weibull_dszi_law_spec()    | full_weibull_dszi_law_spec()
  | weibull_mixture_law_spec() | full_weibull_mixture_law_spec()
  | weibull_zi_law_spec()      | full_weibull_zi_law_spec()

  | beta_2p_law_spec() | full_beta_2p_law_spec()

  | arbitrary_law_spec().
% A user-level specification of a (parametrised) randow law, either natively
% supported or arbitrary.
%
% It is a tuple whose first element is a law identifier.




% Uniform distributions.


-type uniform_law_spec() :: { 'uniform', Min :: number(), Max :: number() }
						  | { 'uniform', Max :: number() }.
% A probability distribution with which all declared samples have the same
% probability of being drawn: will return random floats uniformly distributed
% in [Min,Max] if both bounds are specified, otherwise in [0,Max].


-type full_uniform_law_spec() :: { 'uniform', Min :: float(), Max :: float() }.
% Canonical, most complete uniform law specification.


-type integer_uniform_law_spec() :: { 'integer_uniform', Nmax :: integer() }
									| full_integer_uniform_law_spec().
% A probability distribution with which all declared samples have the same
% probability of being drawn: will return random integers uniformly distributed
% in [Nmin,Nmax] if both bounds are specified, otherwise in [0,Nmax].


-type full_integer_uniform_law_spec() ::
	{ 'integer_uniform', Nmin :: integer(), Nmax :: integer() }.



% Exponential distributions.


-type exponential_1p_law_spec() :: { 'exponential_1p', Lambda :: rate() }.
% The exponential law with one parameter is fully determined when its single,
% "rate" parameter (Lambda>0) is given.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% Refer to https://en.wikipedia.org/wiki/Exponential_distribution.


-type exponential_law_spec() :: { 'exponential', Lambda :: rate() }.
% Alternative to exponential_1p_law_spec/0.


-type positive_integer_exponential_1p_law_spec() ::
		{ 'positive_integer_exponential_1p', Lambda :: rate() }.
% An exponential law with one parameter yielding only positive integer samples.
%
% May be useful for example if wanting to draw duration values.
%
% Refer to exponential_1p_law_spec/0 for further details.


-type exponential_2p_law_spec() ::
		{ 'exponential_2p', Lambda :: rate(), Gamma :: rate() }
	  | { 'exponential_2p', Lambda :: rate(), Gamma :: rate(), sample_count() }
	  | full_exponential_2p_law_spec().
% The exponential law with two parameters is fully determined when its two
% "rate" parameters (Lambda>0 and Gamma>0) are given.
%
% The probability density function is p(x) = Lambda.exp(-Gamma.x).
%
% Refer to https://en.wikipedia.org/wiki/Exponential_distribution and
% https://reliability.readthedocs.io/en/latest/API/Fitters/Fit_Exponential_2P.html.


% No full_exponential_1p_law_spec(), as directly computed (alias method not
% used):
%
-type full_exponential_law_spec() :: full_exponential_2p_law_spec().

-type full_exponential_2p_law_spec() ::
	{ 'exponential_2p', Lambda :: rate(), Gamma :: rate(),
	  sample_count(), bounds() }.
% Canonical, most complete Exponential-2p law specification with two parameters.
% Refer to exponential_2p_law_spec/0 for further details.




% Gamma distributions.
%
% The Gamma distribution (see https://en.wikipedia.org/wiki/Gamma_distribution),
% includes the exponential distribution, the (unrelated) Erlang distribution,
% and the chi-square distribution.


-type full_gamma_law_spec() ::
		full_gamma_2p_law_spec() | full_gamma_3p_law_spec().


-type gamma_2p_law_spec() ::
	{ 'gamma_2p', K :: positive_float(), Theta :: positive_float() }
  | { 'gamma_2p', K :: positive_float(), Theta :: positive_float(),
	  sample_count() }
  | full_gamma_2p_law_spec().
% The Gamma law with two parameters, whose K > 0 is the shape parameter and
% Theta > 0 is the scale parameter.
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Gamma_distribution and
% https://reliawiki.org/index.php/The_Gamma_Distribution.


-type full_gamma_2p_law_spec() ::
	{ 'gamma_2p', K :: positive_float(), Theta :: positive_float(),
	  sample_count(), bounds() }.
% Canonical, most complete Gamma law specification with two parameters.
% Refer to gamma_2p_law_spec/0 for further details.



-type gamma_3p_law_spec() ::
	{ 'gamma_3p', Alpha :: positive_float(), Beta :: positive_float(),
	  Theta :: positive_float() }
  | { 'gamma_3p', Alpha :: positive_float(), Beta :: positive_float(),
	   Theta :: positive_float(), sample_count() }
  | full_gamma_3p_law_spec().
% The Gamma law with three parameters, whose:
%
% - Alpha > 0 is a shape parameter
%
% - Beta > 0 is a shape parameter
%
% - Theta > 0 is the scale parameter
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Gamma_distribution and
% https://reliawiki.org/index.php/The_Gamma_Distribution.


-type full_gamma_3p_law_spec() ::
	{ 'gamma_3p', Alpha :: positive_float(), Beta :: positive_float(),
	  Theta :: positive_float(), sample_count(), bounds() }.
% Canonical, most complete Gamma law specification with three parameters.
% Refer to gamma_3p_law_spec/0 for further details.



% Gaussian distributions.

-type normal_2p_law_spec() ::
	{ 'normal_2p', Mu :: mean(), Sigma :: standard_deviation() }.
% A Gaussian (a.k.a. normal, bell curve) law is fully determined when its two
% parameters are given:
%
% - its mean (Mu; no value restriction), the average value of the samples
%
% - its standard deviation (Sigma>0), being expressed in the same unit as the
% samples (its square being the variance)
%
% About 68% of the samples are in [Mu-Sigma;Mu+Sigma].
% About 95.4% of the samples (i.e. almost all) are in [Mu-2.Sigma;Mu+2.Sigma].
%
% See also: https://en.wikipedia.org/wiki/Normal_distribution.
%
% Such a Gaussian law could be designated as normal_2p as well.


-type gaussian_law_spec() ::
	{ 'gaussian', Mu :: mean(), Sigma :: standard_deviation() }.
% Synonym for normal_2p_law_spec/0.


-type positive_integer_gaussian_law_spec() ::
	{ 'positive_integer_gaussian', Mu :: mean(),
	  Sigma :: standard_deviation() }.
% A Gaussian (a.k.a. normal, bell curve) law yielding only positive integer
% samples.
%
% May be useful for example if wanting to draw duration values.
%
% Refer to gaussian_law/0 for further details.



% Gumbel distribution.


-type gumbel_2p_law_spec() ::
	{ 'gumbel_2p', Mu :: float(), Beta :: positive_float() }
  | { 'gumbel_2p', Mu :: float(), Beta :: positive_float(), sample_count() }
  | full_gumbel_2p_law_spec().
% The Gumbel law, with two parameters: Mu, in R, is the location parameter, and
% Beta > 0 is the scale parameter.
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Gumbel_distribution and
% https://reliawiki.org/index.php/The_Gumbel/SEV_Distribution.


-type full_gumbel_2p_law_spec() ::
	{ 'gumbel_2p', Mu :: float(), Beta :: positive_float(),
	  sample_count(), bounds() }.
% Canonical, most complete Gumbel law specification with two parameters.
%
% Refer to gumbel_2p_law_spec/0 for further details.



% Loglogistic distributions.


-type loglogistic_2p_law_spec() ::
	{ 'loglogistic_2p', Alpha :: positive_float(), Beta :: positive_float() }
  | { 'loglogistic_2p', Alpha :: positive_float(), Beta :: positive_float(),
	  sample_count() }
  | full_loglogistic_2p_law_spec().
% The Log-logistic law, with two parameters: Alpha > 0 (sometimes noted c) is
% the scale parameter, and Beta > 0 (sometimes noted sigma) is the shape
% parameter.
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Log-logistic_distribution and
% https://reliawiki.org/index.php/The_Loglogistic_Distribution.


-type full_loglogistic_2p_law_spec() ::
	{ 'loglogistic_2p', Alpha :: positive_float(), Beta :: positive_float(),
	  sample_count(), bounds() }.
% Canonical, most complete Log-logistic law specification with two parameters.
%
% Refer to loglogistic_2p_law_spec/0 for further details.


-type loglogistic_3p_law_spec() ::
	{ 'loglogistic_3p', Alpha :: positive_float(), Beta :: positive_float(),
	  Theta :: float() }
  | { 'loglogistic_3p', Alpha :: positive_float(), Beta :: positive_float(),
	  Theta :: float(), sample_count() }
  | full_loglogistic_3p_law_spec().
% The Log-logistic law, with three parameters: Alpha > 0 is the scale parameter,
% Beta > 0 is the shape parameter, Theta is the last one.
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Log-logistic_distribution and
% https://reliawiki.org/index.php/The_Loglogistic_Distribution.


-type full_loglogistic_3p_law_spec() ::
	{ 'loglogistic_3p', Alpha :: positive_float(), Beta :: positive_float(),
	  Theta :: positive_float(), sample_count(), bounds() }.
% Canonical, most complete Loglogistic law specification with three parameters.
%
% Refer to loglogistic_3p_law_spec/0 for further details.


-type full_loglogistic_law_spec() ::
		full_loglogistic_2p_law_spec() | full_loglogistic_3p_law_spec().




% Lognormal distribution.


-type lognormal_2p_law_spec() ::
	{ 'lognormal_2p', Mu :: float(), Sigma :: positive_float() }
  | { 'lognormal_2p', Mu :: float(), Sigma :: positive_float(),
	  sample_count() }
  | full_lognormal_2p_law_spec().
% The Log-normal law, with two parameters: Mu, in R (typically the mean of the
% natural logarithms of the times-to-failures), and Sigma > 0 (typically the
% standard deviation of the natural logarithms of the times-to-failure).
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Log-normal_distribution and
% http://reliawiki.org/index.php/The_Lognormal_Distribution.


-type full_lognormal_2p_law_spec() ::
	{ 'lognormal_2p', Mu :: float(), Sigma :: positive_float(),
	  sample_count(), bounds() }.
% Canonical, most complete Log-normal law specification with two parameters.
%
% Refer to lognormal_2p_law_spec/0 for further details.


-type lognormal_3p_law_spec() ::
	{ 'lognormal_3p', Mu :: float(), Sigma :: positive_float(),
	  Theta :: float() }
  | { 'lognormal_3p', Mu :: float(), Sigma :: positive_float(),
	  Theta :: float(), sample_count() }
  | full_lognormal_3p_law_spec().
% The Log-normal law, with three parameters: Mu, in R (typically the mean of the
% natural logarithms of the times-to-failures), Sigma > 0 (typically the
% standard deviation of the natural logarithms of the times-to-failure) and
% Theta, presumably in R.
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Log-normal_distribution and
% http://reliawiki.org/index.php/The_Lognormal_Distribution.


-type full_lognormal_3p_law_spec() ::
	{ 'lognormal_3p', Mu :: float(), Sigma :: positive_float(),
	  Theta :: float(), sample_count(), bounds() }.
% Canonical, most complete Lognormal law specification with three parameters.
%
% Refer to lognormal_3p_law_spec/0 for further details.


-type full_lognormal_law_spec() ::
		full_lognormal_2p_law_spec() | full_lognormal_3p_law_spec().



% Weibull distributions.


-type full_weibull_law_spec() ::
	full_weibull_2p_law_spec()   | full_weibull_3p_law_spec()
  | full_weibull_cr_law_spec()   | full_weibull_ds_law_spec()
  | full_weibull_dszi_law_spec() | full_weibull_mixture_law_spec()
  | full_weibull_zi_law_spec().


-type weibull_2p_law_spec() ::
	{ 'weibull_2p', K :: positive_float(), Lambda :: positive_float() }
  | { 'weibull_2p', K :: positive_float(), Lambda :: positive_float(),
	  sample_count() }
  | full_weibull_2p_law_spec().
% The Weibull law with two parameters, whose K > 0 is the shape parameter
% (sometimes named beta, or slope) and Lambda > 0 is the scale parameter
% (sometimes named alpha, or eta, or characteristic life).
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Weibull_distribution and
% https://reliawiki.org/index.php/The_Weibull_Distribution.


-type full_weibull_2p_law_spec() ::
	{ 'weibull_2p', K :: positive_float(), Lambda :: positive_float(),
	  sample_count(), bounds() }.
% Canonical, most complete Weibull law specification with two parameters.
%
% Refer to weibull_2p_law_spec/0 for further details.



-type weibull_3p_law_spec() ::
	{ 'weibull_3p', K :: positive_float(), Lambda :: positive_float(),
	  Gamma :: float() }
  | { 'weibull_3p', K :: positive_float(), Lambda :: positive_float(),
	  Gamma :: float(), sample_count() }
  | full_weibull_3p_law_spec().
% The Weibull law with three parameters, whose:
%
% - K > 0 is the shape parameter (sometimes named beta, or slope)
%
% - Lambda > 0 is the scale parameter (sometimes named alpha, or eta, or
% characteristic life)
%
% - Gamma (in R) is the location parameter (or failure free life)
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Weibull_distribution and
% https://reliawiki.org/index.php/The_Weibull_Distribution.


-type full_weibull_3p_law_spec() ::
	{ 'weibull_3p', K :: positive_float(), Lambda :: positive_float(),
	  Gamma :: float(), sample_count(), bounds() }.
% Canonical, most complete Weibull law specification with three parameters.
%
% Refer to weibull_3p_law_spec/0 for further details.



-type weibull_cr_law_spec() ::
	{ 'weibull_cr', Lambda :: positive_float(), K :: positive_float(),
	  Theta :: positive_float() }
  | { 'weibull_cr', Lambda :: positive_float(), K :: positive_float(),
	  Theta :: positive_float(), sample_count() }
  | full_weibull_cr_law_spec().
% The Weibull CR ("Competing Risks") law, with three parameters: Lambda > 0, K >
% 0 and Theta > 0.
%
% A sample count and specific bounds can be specified.
%
% See also
% https://reliability.readthedocs.io/en/latest/API/Distributions/Competing_Risks_Model.html


-type full_weibull_cr_law_spec() ::
	{ 'weibull_cr', Lambda :: positive_float(), K :: positive_float(),
	  Theta :: positive_float(), sample_count(), bounds() }.
% Canonical, most complete Weibull CR law specification, with three parameters.
%
% Refer to weibull_cr_law_spec/0 for further details.



-type weibull_ds_law_spec() ::
	{ 'weibull_ds', Lambda :: positive_float(), K :: positive_float(),
	  Sigma :: positive_float() }
  | { 'weibull_ds', Lambda :: positive_float(), K :: positive_float(),
	  Sigma :: positive_float(), sample_count() }
  | full_weibull_ds_law_spec().
% The Weibull DS ("Defective Subpopulation") law, with three parameters: Lambda
% > 0, K > 0 and Sigma > 0.
%
% A sample count and specific bounds can be specified.
%
% See also
% https://reliability.readthedocs.io/en/latest/API/Distributions/DSZI_Model.html,
% which discusses DS as well.


-type full_weibull_ds_law_spec() ::
	{ 'weibull_ds', Lambda :: positive_float(), K :: positive_float(),
	  Sigma :: positive_float(), sample_count(), bounds() }.
% Canonical, most complete Weibull DS law specification, with three parameters.
%
% Refer to weibull_ds_law_spec/0 for further details.



-type weibull_dszi_law_spec() ::
	{ 'weibull_dszi', Lambda :: positive_float(), K :: positive_float(),
	  Sigma :: positive_float(), Theta :: positive_float() }
  | { 'weibull_dszi', Lambda :: positive_float(), K :: positive_float(),
	  Sigma :: positive_float(), Theta :: positive_float(), sample_count() }
  | full_weibull_dszi_law_spec().
% The Weibull DSZI ("Defective Subpopulation" / "Zero Inflated") law, with four
% parameters: Lambda > 0, K > 0, Sigma > 0 and Theta > 0.
%
% A sample count and specific bounds can be specified.
%
% See also
% https://reliability.readthedocs.io/en/latest/API/Distributions/DSZI_Model.html.


-type full_weibull_dszi_law_spec() ::
	{ 'weibull_dszi', Lambda :: positive_float(), K :: positive_float(),
	  Sigma :: positive_float(), Theta :: positive_float(), sample_count(),
	  bounds() }.
% Canonical, most complete Weibull DSZI law specification, with four parameters.
%
% Refer to weibull_dszi_law_spec/0 for further details.



-type weibull_mixture_law_spec() ::
	{ 'weibull_mixture', P :: positive_float(),
	  Lambda1 :: positive_float(), K1 :: positive_float(),
	  Lambda2 :: positive_float(), K2 :: positive_float() }
  | { 'weibull_mixture', P :: positive_float(),
	  Lambda1 :: positive_float(), K1 :: positive_float(),
	  Lambda2 :: positive_float(), K2 :: positive_float(), sample_count() }
  | full_weibull_mixture_law_spec().
% The Weibull mixture law, a combination of two weibull_2p distributions, with a
% total of five parameters.
%
% A sample count and specific bounds can be specified.
%
% See also https://reliability.readthedocs.io/en/latest/Mixture%20models.html


-type full_weibull_mixture_law_spec() ::
	{ 'weibull_mixture', P :: positive_float(),
	  Lambda1 :: positive_float(), K1 :: positive_float(),
	  Lambda2 :: positive_float(), K2 :: positive_float(),
	  sample_count(), bounds() }.
% Canonical, most complete Weibull mixture law specification, with five
% parameters.
%
% Refer to weibull_mixture_law_spec/0 for further details.



-type weibull_zi_law_spec() ::
	{ 'weibull_zi', Lambda :: positive_float(), K :: positive_float(),
	  P :: positive_float() }
  | { 'weibull_zi', Lambda :: positive_float(), K :: positive_float(),
	  P :: positive_float(), sample_count() }
  | full_weibull_zi_law_spec().
% The Weibull ZI ("Zero Inflated") law with three parameters.
%
% A sample count and specific bounds can be specified.
%
% See also
% https://reliability.readthedocs.io/en/latest/API/Distributions/DSZI_Model.html,
% which discusses ZI as well.


-type full_weibull_zi_law_spec() ::
	{ 'weibull_zi', Lambda :: positive_float(), K :: positive_float(),
	  P :: positive_float(), sample_count(), bounds() }.
% Canonical, most complete Weibull ZI law specification with three parameters.
%
% Refer to weibull_zi_law_spec/0 for further details.






% Beta distributions.


-type beta_2p_law_spec() ::
	{ 'beta_2p', Alpha :: positive_float(), Beta :: positive_float() }
  | { 'beta_2p', Alpha :: positive_float(), Beta :: positive_float(),
	  sample_count() }
  | full_beta_2p_law_spec().
% The Beta law (of the first kind) with two shape parameters, Alpha > 0 and Beta
% > 0.
%
% A sample count and specific bounds can be specified, generally on the [0,1]
% interval.
%
% See also https://en.wikipedia.org/wiki/Beta_distribution and
% https://reliability.readthedocs.io/en/latest/Equations%20of%20supported%20distributions.html#beta-distribution


-type full_beta_law_spec() :: full_beta_2p_law_spec().

-type full_beta_2p_law_spec() ::
	{ 'beta_2p', Alpha :: positive_float(), Beta :: positive_float(),
	  sample_count(), bounds() }.
% Canonical, most complete Beta law specification with two parameters.
% Refer to beta_2p_law_spec/0 for further details.


-type pdf_info() :: pdf()
				  | { pdf(), sample_count() }
				  | full_pdf_info().
% Information regarding a PDF to be used in order to define an arbitrary law.


-type full_pdf_info() :: { pdf(), sample_count(), SampleBounds :: bounds() }.
% Most complete information regarding a PDF to be used in order to define an
% arbitrary law.



-type arbitrary_law_spec() ::
	{ 'arbitrary', Name :: any_string(),
	  discrete_probability_distribution() | full_pdf_info() }.
% User-level specification of an arbitrary random law.
%
% It includes its name, and one way of performing a reverse sampling of it:
%  - either directly from discrete, user-specified samples
%  - or through its Probability Density Function (which will then be
%  discretised) and any relevant parameters that will be applied to it (stored
%  so that they remain available afterwards); its intended sampling may be
%  specified



% Subsection regarding the runtime data of laws.
%
% They are often the same as their user specification, except for arbitrary
% laws, which embed additionally their precomputed alias table.




-type random_law_settings() :: tuple().
% The internal, law-specific settings of a law.
%
% The first element of the tuple should be the name of the law, whereas its last
% elements shall correspond to the sampling information:
%  {law_name(), ..., sample_count(), bounds() }.


-type random_law_data() ::

  % The general rule:
  { random_law_settings(), maybe( alias_table() ) }

  | uniform_law_data()
  | integer_uniform_law_data()

  | exponential_law_data()
  | positive_integer_exponential_law_data()

  | gamma_law_data()

  | gumbel_law_data()

  | loglogistic_law_data()

  | lognormal_law_data()

  | gaussian_law_data()
  | positive_integer_gaussian_law_data()

  | weibull_law_data()

  | beta_law_data()

  | arbitrary_law_data().
% Static, runtime data associated to an actual random law, so that samples can
% be obtained from it.
%
% The runtime data of a law is often mostly the same as its user specification
% (except some transformations in some cases), except for laws obtained through
% discretisation of their PDF, which embed additionally their precomputed alias
% table.
%
% This is not a random state, which is a mutable internal state of a random
% generator (see random_state/0).


-type uniform_law_data() :: { full_uniform_law_spec(), 'undefined' }.

-type integer_uniform_law_data() ::
		{ full_integer_uniform_law_spec(), 'undefined' }.



-type exponential_1p_law_data() :: 
		{ exponential_1p_law_spec() | exponential_law_spec(), 'undefined' }.

-type exponential_2p_law_data() ::
		{ full_exponential_2p_law_spec(), alias_table() }.

-type exponential_law_data() :: exponential_1p_law_data()
							  | exponential_2p_law_data().


-type positive_integer_exponential_1p_law_data() ::
		{ positive_integer_exponential_1p_law_spec(), 'undefined' }.

-type positive_integer_exponential_law_data() ::
		positive_integer_exponential_1p_law_data().



-type gamma_2p_law_settings() :: { 'gamma_2p', K :: positive_float(),
		Theta :: positive_float(), sample_count(), bounds() }.
% Internal settings of the Gamma-2p laws.

-type gamma_3p_law_settings() :: { 'gamma_3p', Alpha :: positive_float(),
		Beta :: positive_float(), Theta :: positive_float(), sample_count(),
		bounds() }.
% Internal settings of the Gamma-3p laws.


-type gamma_2p_law_data() :: { gamma_2p_law_settings(), alias_table() }.
-type gamma_3p_law_data() :: { gamma_3p_law_settings(), alias_table() }.


-type gamma_law_data() :: gamma_2p_law_data() | gamma_3p_law_data().
% A bit like gamma_law_spec/0, except that its precomputed alias table is
% stored as well.



-type gumbel_2p_law_settings() :: { 'gumbel_2p', Mu :: float(),
		Beta :: positive_float(), sample_count(), bounds() }.
% Internal settings of the Gumbel-2p laws.

-type gumbel_2p_law_data() :: { gumbel_2p_law_settings(), alias_table() }.
% A bit like gumbel_2p_law_spec/0, except that its precomputed alias table is
% stored as well.


-type gumbel_law_data() :: gumbel_2p_law_data().



-type loglogistic_2p_law_settings() ::
	{ 'loglogistic_2p', Alpha :: positive_float(), Beta :: positive_float(),
	  sample_count(), bounds() }.
% Internal settings of the Loglogistic-2p laws.

-type loglogistic_2p_law_data() ::
		{ loglogistic_2p_law_settings(), alias_table() }.
% A bit like loglogistic_2p_law_spec/0, except that its precomputed alias table
% is stored as well.


-type loglogistic_3p_law_settings() ::
	{ 'loglogistic_3p', Alpha :: positive_float(), Beta :: positive_float(),
	  Theta :: positive_float(), sample_count(), bounds() }.
% Internal settings of the Loglogistic-3p laws.

-type loglogistic_3p_law_data() ::
		{ loglogistic_3p_law_settings(), alias_table() }.
% A bit like loglogistic_3p_law_spec/0, except that its precomputed alias table
% is stored as well.


-type loglogistic_law_data() :: loglogistic_2p_law_data()
							  | loglogistic_3p_law_data().



-type lognormal_2p_law_settings() ::
	{ 'lognormal_2p', Mu :: float(), Sigma :: positive_float(),
	  sample_count(), bounds() }.
% Internal settings of the Lognormal-2p laws.

-type lognormal_2p_law_data() ::
		{ lognormal_2p_law_settings(), alias_table() }.
% A bit like lognormal_2p_law_spec/0, except that its precomputed alias table
% is stored as well.


-type lognormal_3p_law_settings() ::
	{ 'lognormal_3p', Mu :: float(), Sigma :: positive_float(),
	  Theta :: float(), sample_count(), bounds() }.
% Internal settings of the Lognormal-3p laws.

-type lognormal_3p_law_data() :: { lognormal_3p_law_settings(), alias_table() }.
% A bit like lognormal_3p_law_spec/0, except that its precomputed alias table
% is stored as well.


-type lognormal_law_data() :: lognormal_2p_law_data() | lognormal_3p_law_data().



-type gaussian_law_data() :: { gaussian_law_spec(), 'undefined' }.

-type positive_integer_gaussian_law_data() ::
		{ positive_integer_gaussian_law_spec(), 'undefined' }.



-type weibull_2p_law_settings() :: { 'weibull_2p', K :: positive_float(),
		Lambda :: positive_float(), sample_count(), bounds() }.
% Internal settings of the Weibull-2p laws.


-type weibull_3p_law_settings() :: { 'weibull_3p', K :: positive_float(),
		Lambda :: positive_float(), Gamma :: float(), sample_count(),
		bounds() }.
% Internal settings of the Weibull-3p laws.


-type weibull_cr_law_settings() :: { 'weibull_cr', Lambda :: positive_float(),
		K :: positive_float(), Theta :: positive_float(), sample_count(),
		bounds() }.
% Internal settings of the Weibull-CR laws.


-type weibull_ds_law_settings() :: { 'weibull_ds', Lambda :: positive_float(),
		K :: positive_float(), Sigma :: positive_float(), sample_count(),
		bounds() }.
% Internal settings of the Weibull-DS laws.


-type weibull_dszi_law_settings() :: { 'weibull_dszi',
		Lambda :: positive_float(), K :: positive_float(),
		Sigma :: positive_float(), Theta :: positive_float(), sample_count(),
		bounds() }.
% Internal settings of the Weibull-DSZI laws.


-type weibull_mixture_law_settings() :: { 'weibull_mixture',
		P :: positive_float(),
		Lambda1 :: positive_float(), K1 :: positive_float(),
		Lambda2 :: positive_float(), K2 :: positive_float(), bounds() }.
% Internal settings of the Weibull-mixture laws.


-type weibull_zi_law_settings() :: { 'weibull_zi', Lambda :: positive_float(),
		K :: positive_float(), P :: positive_float(), sample_count(),
		bounds() }.
% Internal settings of the Weibull-ZI laws.



-type weibull_2p_law_data() :: { weibull_2p_law_settings(), alias_table() }.
% A bit like weibull_2p_law_spec/0, except that its precomputed alias table is
% stored as well.

-type weibull_3p_law_data() :: { weibull_3p_law_settings(), alias_table() }.
% A bit like weibull_3p_law_spec/0, except that its precomputed alias table is
% stored as well.

-type weibull_cr_law_data() :: { weibull_cr_law_settings(), alias_table() }.
% A bit like weibull_cr_law_spec/0, except that its precomputed alias table is
% stored as well.

-type weibull_ds_law_data() :: { weibull_ds_law_settings(), alias_table() }.
% A bit like weibull_ds_law_spec/0, except that its precomputed alias table is
% stored as well.

-type weibull_dszi_law_data() :: { weibull_dszi_law_settings(), alias_table() }.
% A bit like weibull_dszi_law_spec/0, except that its precomputed alias table is
% stored as well.

-type weibull_mixture_law_data() ::
		{ weibull_mixture_law_settings(), alias_table() }.
% A bit like weibull_mixture_law_spec/0, except that its precomputed alias table
% is stored as well.

-type weibull_zi_law_data() :: { weibull_zi_law_settings(), alias_table() }.
% A bit like weibull_zi_law_spec/0, except that its precomputed alias table is
% stored as well.


-type weibull_law_data() ::
		weibull_2p_law_data()   | weibull_3p_law_data()
	  | weibull_cr_law_data()   | weibull_ds_law_data()
	  | weibull_dszi_law_data() | weibull_mixture_law_data()
	  | weibull_zi_law_data().



-type beta_2p_law_settings() :: { 'beta_2p', Alpha :: positive_float(),
		Beta :: positive_float(), sample_count(), bounds() }.
% Internal settings of the Beta law.


-type beta_law_data() :: beta_2p_law_data().

-type beta_2p_law_data() :: { beta_2p_law_settings(), alias_table() }.
% A bit like beta_2p_law_spec/0, except that its precomputed alias table is
% stored as well.




-type arbitrary_law_pseudo_spec() ::
		{ 'arbitrary', Name :: bin_string(), sample_count(),
		  maybe( bounds() ) }.
% A (pseudo) specification for an arbitrary law, to be stored in a law data.
%
%

-type arbitrary_law_data() :: { arbitrary_law_pseudo_spec(), alias_table() }.
% The definition of an arbitrary law.
%
% Quite like arbitrary_law_spec/0, except for the string type, the PDF that is
% dropped, any parameters that are kept for further reference, and an additional
% precomputed alias table stored.



-type discrete_probability_distribution( T ) :: [ sample_entry( T ) ].
% The specification of a discrete probability distribution (a.k.a. frequency
% distribution) whose samples are of the specified type.
%
% For example discrete_probability_distribution(integer()) or
% discrete_probability_distribution(vector3:vector3()).
%
% Samples of null probability are usually omitted, as such a probability is
% implicit and results in the corresponding sample never to be drawn.
%
% At least an entry with a non-null probability must be defined for any drawing
% to occur.
%
% Preferably a given sample value is specified only once, i.e. is declared in a
% single entry (otherwise the distribution will behave as if the probabilities
% for that sample were summed - yet the distribution will be less compact).
%
% Such a distribution can be obtained either directly or by sampling a fun(T ->
% probability_like()) function over at least a part of its domain.
%
% It may no be normalised.


-type discrete_probability_distribution() ::
		discrete_probability_distribution( any() ).
% The specification of a discrete probability distribution of unknown sample
% type.
%
% For example: [{'red', 0.1}, {'blue', 0.2}, {'green', 0.6}].


-type pdf( S ) :: fun( ( S ) -> probability_like() ).
% A Probability Density Function telling, for a given sample of type S, its
% corresponding probability-like value.
%
% See also the math_utils:sample* functions and get_samples_from/2.


-type pdf() :: pdf( any() ).
% A Probability Density Function for samples of unspecified type.


-type exponential_pdf() :: exponential_1p_pdf() | exponential_2p_pdf().
% A PDF of an exponential distribution.

-type exponential_1p_pdf() :: pdf().
% A PDF of the exponential distribution with one parameter.

-type exponential_2p_pdf() :: pdf().
% A PDF of the exponential distribution with two parameters.



-type gamma_pdf() :: gamma_2p_pdf() | gamma_3p_pdf().
% A PDF of a Gamma distribution.

-type gamma_2p_pdf() :: pdf().
% A PDF of the two-parameter Gamma distribution.

-type gamma_3p_pdf() :: pdf().
% A PDF of the three-parameter Gamma distribution.


-type gumbel_pdf() :: gumbel_2p_pdf().
% A PDF of a Gumbel distribution.

-type gumbel_2p_pdf() :: pdf().
% A PDF of the two-parameter Gumbel distribution.



-type loglogistic_pdf() :: loglogistic_2p_pdf() | loglogistic_3p_pdf().
% A PDF of a Log-logistic distribution.


-type loglogistic_2p_pdf() :: pdf().
% A PDF of the two-parameter Log-logistic distribution.

-type loglogistic_3p_pdf() :: pdf().
% A PDF of the three-parameter Log-logistic distribution.



-type lognormal_pdf() :: lognormal_2p_pdf() | lognormal_3p_pdf().
% A PDF of a Log-normal distribution.


-type lognormal_2p_pdf() :: pdf().
% A PDF of the two-parameter Log-normal distribution.

-type lognormal_3p_pdf() :: pdf().
% A PDF of the three-parameter Log-normal distribution.


-type gaussian_pdf() :: pdf().
% A PDF of a Gaussian distribution.
%
% Could be named normal_2p_pdf() as well.


-type weibull_pdf() ::
	weibull_2p_pdf()   | weibull_3p_pdf()
  | weibull_cr_pdf()   | weibull_ds_pdf()
  | weibull_dszi_pdf() | weibull_mixture_pdf()
  | weibull_zi_pdf().
% A PDF of a Weibull distribution.


-type weibull_2p_pdf() :: pdf().
% A PDF of the two-parameter Weibull distribution.

-type weibull_3p_pdf() :: pdf().
% A PDF of the three-parameter Weibull distribution.

-type weibull_cr_pdf() :: pdf().
% A PDF of the Weibull-CR distribution.

-type weibull_ds_pdf() :: pdf().
% A PDF of the Weibull-DS distribution.

-type weibull_dszi_pdf() :: pdf().
% A PDF of the Weibull-DSZI distribution.

-type weibull_mixture_pdf() :: pdf().
% A PDF of the Weibull-mixture distribution.

-type weibull_zi_pdf() :: pdf().
% A PDF of the Weibull-ZI distribution.


-type beta_pdf() :: beta_2p_pdf().

-type beta_2p_pdf() :: pdf().
% A PDF of the two-parameter Beta distribution.



-export_type([ seed_element/0, seed/0, random_state/0, alias_table/0,
			   sample/0, sample/1, float_sample/0, positive_float_sample/0,
			   sample_count/0,

			   increment/0,
			   %discrete_sampling_info/0, interval_sampling_info/0,
			   %sampling_info/0,

			   sample_entry/0, sample_entry/1,
			   rate/0, shape/0, scale/0, mean/0, standard_deviation/0,
			   law_name/0 ]).


-export_type([ discrete_probability_distribution/0,
			   discrete_probability_distribution/1,
			   pdf/0, pdf/1,

			   exponential_pdf/0, exponential_1p_pdf/0, exponential_2p_pdf/0,

			   gamma_pdf/0, gamma_2p_pdf/0, gamma_3p_pdf/0,
			   gumbel_pdf/0, gumbel_2p_pdf/0,

			   loglogistic_pdf/0, loglogistic_2p_pdf/0, loglogistic_3p_pdf/0,

			   lognormal_pdf/0, lognormal_2p_pdf/0, lognormal_3p_pdf/0,

			   gaussian_pdf/0,

			   weibull_pdf/0, weibull_2p_pdf/0, weibull_3p_pdf/0,
			   weibull_cr_pdf/0, weibull_ds_pdf/0,
			   weibull_dszi_pdf/0, weibull_mixture_pdf/0, weibull_zi_pdf/0,

			   beta_pdf/0, beta_2p_pdf/0 ]).


% Law specs:
-export_type([ random_law_spec/0,

			   uniform_law_spec/0, full_uniform_law_spec/0,
			   integer_uniform_law_spec/0,

			   exponential_1p_law_spec/0, exponential_law_spec/0,
			   positive_integer_exponential_1p_law_spec/0,
			   exponential_2p_law_spec/0,
			   full_exponential_law_spec/0, full_exponential_2p_law_spec/0,

			   gamma_2p_law_spec/0, gamma_3p_law_spec/0,
			   full_gamma_law_spec/0,
			   full_gamma_2p_law_spec/0, full_gamma_3p_law_spec/0,

			   beta_2p_law_spec/0, full_beta_law_spec/0,
			   full_beta_2p_law_spec/0,

			   gumbel_2p_law_spec/0, full_gumbel_2p_law_spec/0,

			   loglogistic_2p_law_spec/0, full_loglogistic_2p_law_spec/0,
			   loglogistic_3p_law_spec/0, full_loglogistic_3p_law_spec/0,

			   lognormal_2p_law_spec/0, full_lognormal_2p_law_spec/0,
			   lognormal_3p_law_spec/0, full_lognormal_3p_law_spec/0,

			   gaussian_law_spec/0, normal_2p_law_spec/0,
			   positive_integer_gaussian_law_spec/0,

			   weibull_2p_law_spec/0,      full_weibull_2p_law_spec/0,
			   weibull_3p_law_spec/0,      full_weibull_3p_law_spec/0,
			   weibull_cr_law_spec/0,      full_weibull_cr_law_spec/0,
			   weibull_ds_law_spec/0,      full_weibull_ds_law_spec/0,
			   weibull_dszi_law_spec/0,    full_weibull_dszi_law_spec/0,
			   weibull_mixture_law_spec/0, full_weibull_mixture_law_spec/0,
			   weibull_zi_law_spec/0,      full_weibull_zi_law_spec/0,
			   full_weibull_law_spec/0,

			   arbitrary_law_spec/0 ]).


% Law runtime data:
-export_type([ random_law_settings/0, random_law_data/0,
			   uniform_law_data/0, integer_uniform_law_data/0,

			   exponential_1p_law_data/0,
			   positive_integer_exponential_1p_law_data/0,
			   exponential_2p_law_data/0,

			   gamma_law_data/0, gamma_2p_law_data/0, gamma_3p_law_data/0,

			   gumbel_2p_law_data/0,

			   loglogistic_law_data/0,
			   loglogistic_2p_law_data/0, loglogistic_3p_law_data/0,

			   lognormal_law_data/0,
			   lognormal_2p_law_data/0, lognormal_3p_law_data/0,

			   gaussian_law_data/0, positive_integer_gaussian_law_data/0,

			   weibull_law_data/0,
			   weibull_2p_law_data/0, weibull_3p_law_data/0,
			   weibull_cr_law_data/0, weibull_ds_law_data/0,
			   weibull_dszi_law_data/0, weibull_mixture_law_data/0,
			   weibull_zi_law_data/0,

			   beta_2p_law_data/0,
			   arbitrary_law_data/0 ]).


% The default number of steps used to discretise a PDF:
%-define( default_pdf_sample_count, 512 ).
-define( default_pdf_sample_count, 1024 ).
%-define( default_pdf_sample_count, 4096 ).


% Implementation notes.

% About pseudorandom number generator (PRNG):
%
% If use_crypto_module is defined, the (now deprecated here, for this use)
% crypto module will be used, otherwise the rand module (which is the default
% now) will be used instead (the random module being now deprecated).
%
% Currently the crypto module is not used by default, as:
%
% - not all Erlang VMs can be built with the proper SSH support
%
% - it is unclear whether the crypto module can be seeded like the random module
% can be (probably it cannot be)
%
% - there is no crypto function returning a random float uniformly distributed
% between 0.0 and 1.0, and it may not be easy to implement it from what is
% available
%
% - we do not know whether the seed is per-node (most likely), or per-process
%
% Therefore crypto cannot be easily swapped with other random generators.
%
% Finally, the requirements of a Cryptographically-secure PRNG (CSPRNG) exceed
% the general PRNGs, and may be useless / less appropriate / of higher costs in
% other contexts. Refer to our hash_utils module for more information on that
% topic.
%
% The current module considered using TinyMT and/or SFMT, yet now they have been
% superseded by the xorshift, xoroshiro, and xoshiro algorithms by Sebastiano
% Vigna, and the rand module offers variations of the xorshift and xoroshiro
% algorithms, called exsss and exro928ss.
%
% Of course, switching random engines will generate different random series.
%
% They may also have different behaviours (e.g. with regards to processes not
% being explicitly seeded, inheriting from a seed that is constant or not - the
% shortest path to break reproducibility).
%
% Note that modules relying on an implicit state (e.g. for seeding) generally
% use the process dictionary to store it (e.g. 'rand').

% Some distributions (e.g. the Gaussian/normal ones) may be defined either
% directly from an uniform distribution, or as any arbitrary function through
% inverse sample. We prefer the former to the latter, which would have
% nevertheless been possible thanks to:
%
% normal_2p: f(x; μ, σ) = (1 / (σ*sqrt(2π))) * e^-((x - μ)^2 / (2*σ^2))
%
% One shall instead use either the {gaussian, Mu, Sigma} law specification or
% the {positive_integer_gaussian, Mu, Sigma} one.

%-define(use_crypto_module,).




% For defines like sqrt_2_pi:
-include("math_utils.hrl").


% Shorthands:

-type array( T ) :: array:array( T ).

-type count() :: basic_utils:count().
-type positive_index() :: basic_utils:positive_index().


-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().


-type probability_like() :: math_utils:probability_like().
% Any number that can be interpreted ultimately as a probability, at least
% relatively to others.
%
% Notably they may not be normalised (with their sum differing from 1.0).


-type bounds() :: math_utils:bounds().

-type probability() :: math_utils:probability().

% A float that is strictly positive:
-type positive_float() :: type_utils:positive_float().
%-type non_negative_float() :: type_utils:non_negative_float().


-import( math, [ pi/0, exp/1, sqrt/1, pow/2 ] ).
-import( math_utils, [ ln/1 ] ).


% Apparently, as soon as functions are defined within preprocessor guards, their
% definition is ignored by edoc, resulting in edoc failing because of multiple
% 'doc' tags at the first function defined outside of these guards.
%
% As, if we copied their documentation there, this would not work either (the
% corresponding functions not being found), we just disabled their 'doc' tags
% ('at' doc being replaced with 'doc:').


% Specs gathered here, because of macro guards.
-spec start_random_source( seed_element(), seed_element(), seed_element() ) ->
								random_state().

-spec start_random_source( 'default_seed' | 'time_based_seed' | seed() ) ->
								void().

-spec can_be_seeded() -> boolean().

-spec reset_random_source( 'default_seed' | 'time_based_seed' | seed() ) ->
								void().

-spec stop_random_source() -> void().


-spec get_uniform_value() -> float().

-spec get_uniform_value( pos_integer() ) -> pos_integer().

-spec get_uniform_value( integer(), integer() ) -> integer().

-spec get_uniform_floating_point_value( number() ) -> float().
-spec get_uniform_floating_point_value( number(), number() ) -> float().

-spec get_random_state() -> maybe( random_state() ).
-spec set_random_state( random_state() ) -> void().



% @doc Generates a list of Count elements uniformly drawn in [1,N].
-spec get_uniform_values( pos_integer(), sample_count() ) -> [ pos_integer() ].
get_uniform_values( N, Count ) ->
	get_uniform_values_helper( N, Count, _Acc=[] ).


get_uniform_values_helper( _N, _Count=0, Acc ) ->
	Acc;

get_uniform_values_helper( N, Count, Acc ) ->
	get_uniform_values_helper( N, Count-1, [ get_uniform_value( N ) | Acc ] ).



% @doc Generates a list of Count elements uniformly drawn in [Nmin,Nmax].
-spec get_uniform_values( integer(), integer(), sample_count() ) ->
			[ integer() ].
get_uniform_values( Nmin, Nmax, Count ) ->
	get_uniform_values_helper( Nmin, Nmax, Count, _Acc=[] ).


% (helper)
get_uniform_values_helper( _Nmin, _Nmax, _Count=0, Acc ) ->
	Acc;

get_uniform_values_helper( Nmin, Nmax, Count, Acc ) ->
	get_uniform_values_helper( Nmin, Nmax, Count - 1,
							   [ get_uniform_value( Nmin, Nmax ) | Acc ] ).


% To test compilation:
%-define(use_crypto_module,).


% Now crypto is hardly used anymore; however these are the doc tags that edoc
% reads.
%
-ifdef(use_crypto_module).


% crypto module used here.
%
% Warning: the seed and state management are presumably global (not
% per-process).


% @doc Starts the random source with specified seeding.
start_random_source( _A, _B, _C ) ->
	throw( crypto_module_cannot_be_seeded ).



% @doc Starts the random source with specified seeding.
start_random_source( default_seed ) ->

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "~w starting random source with crypto.",
							  [ self() ] ) ),

	ok = crypto:start();

start_random_source( time_based_seed ) ->
	throw( crypto_module_cannot_be_seeded ).


% @doc Tells whether this random source can be seeded.
%
% crypto cannot be seeded, but rand can.
%
can_be_seeded() ->
	false.


% @doc Resets the random source with a new seed.
reset_random_source( _Seed ) ->
	throw( crypto_module_cannot_be_reset ).


% @doc Stops the random source.
stop_random_source() ->
	ok = crypto:stop().



% @doc Returns a random float uniformly distributed between 0.0 (included) and
% 1.0 (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value() ->
	% Not available: crypto:rand_uniform( 0.0, 1.0 ).
	throw( not_available ).



% @doc Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( N ) ->
	crypto:rand_uniform( 1, N+1 ).



% @doc Returns an integer random value generated from an uniform distribution in
% [Nmin,Nmax] (thus with both bounds included), updating the random state in the
% process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( Nmin, Nmax ) when Nmin =< Nmax ->
	crypto:rand_uniform( Nmin, Nmax+1 ).


% @doc Returns a floating-point random value in [0.0,N[ generated from an
% uniform distribution.
%
% Given a number (integer or float) N (positive or not), returns a random
% floating-point value uniformly distributed between 0.0 (included) and N
% (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( N ) ->
	throw( not_available ).


% @doc Returns a floating-point random value in [Nmin, Nmax[ generated from an
% uniform distribution.
%
% Given two numbers (integer or float) Nmin and Nmax (each being positive or
% not), returns a random floating-point value uniformly distributed between Nmin
% (included) and Nmax (excluded), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( Nmin, Nmax ) ->
	throw( not_available ).



% @doc Returns the name of the module managing the random generation.
%
% Spec already specified, for all random settings.
%
-spec get_random_module_name() -> 'crypto'.
get_random_module_name() ->
	crypto.



% @doc Returns the random state of this process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
get_random_state() ->
	% At least: not implemented yet.
	throw( not_available ).



% @doc Sets the random state of this process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
set_random_state( _NewState ) ->
	% At least: not implemented yet.
	throw( not_available ).





-else. % use_crypto_module not defined below:


% Here we do not use the 'crypto' module; we use the 'rand' one (replacing the
% deprecated 'random' module), and this is the default, most common setting now.


% For the 'random' module, according to
% http://osdir.com/ml/erlang-questions-programming/2013-10/msg00235.html one
% must ensure that the initial values are large and different enough.
%
% Anyway, starting from OTP 18.0, we switched from the 'random' module to the
% new 'rand' module, offering better service and control.
%
% They have different semantics though: if a process is not explicitly seeded,
% 'random' will assign a constant seed while 'rand' will assign a non-constant
% one.

% Default rand module used here.
%
% The seed and state management is per-process. We prefer that. The process
% dictionary (based on the 'rand_seed' key) is used to store this random state
% (otherwise we stay away from any use of this impure dictionary).



% Refer to http://www.erlang.org/doc/man/rand.html for more information about
% algorithms:

% Xorshift116+, 58 bits precision and period of 2^116-1, 320 bytes per state, on
% 64-bit:
%
% (corrected version of exsplus, yet now superseded by exrop, see below)
%
%-define( rand_algorithm, exsp ).



% Xoroshiro116+, 58 bits precision and period of 2^116-1
%
% Jump function: equivalent to 2^64 calls.
%
% Default since OTP 20, to be used in most cases:
%
-define( rand_algorithm, exrop ).



% Xorshift64*, 64 bits precision and a period of 2^64-1, 336 bytes per state on
% 64-bit:
%
%-define( rand_algorithm, exs64 ).


% Xorshift1024*, 64 bits precision and a period of 2^1024-1 (most expensive of
% the built-in random algorithms, 856 bytes per state on 64-bit):
%
% (corrected version, to be used instead of exsplus)
%
%-define( rand_algorithm, exs1024s ).



% doc: Starts the random source with specified seeding.
%
% Note: if a process does not explicitly select a seed, with 'rand' a
% non-constant seed will be assigned. For reproducibility, start your random
% sources with a seed of your own.
%
start_random_source( A, B, C ) ->

	Seed = { A, B, C },

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "~w starting random source with rand (~p), "
			"seeded with ~w.", [ self(), ?rand_algorithm, Seed ] ) ),

	%random:seed( A, B, C ).
	rand:seed( ?rand_algorithm, Seed ).



% doc: Seeds the random number generator, with specified seeding, or with a
% default seed (if wanting to obtain the same random series at each run) or with
% current time (if wanting "real", non-reproducible randomness).
%
% Spec already specified, for all random settings.
%
% Note: if a process does not explicitly select a seed, with 'rand' a
% non-constant seed will be assigned. For reproducibility, start explicitly your
% random sources.
%
start_random_source( _Seed={ A, B, C } ) ->
	start_random_source( A, B, C );


start_random_source( default_seed ) ->

	% Use default (fixed) values in the process dictionary:
	%random:seed();

	% random:seed/0 was using a constant seed, not rand:seed/1, so we have to
	% provide a constant seed by ourselves:
	%
	ConstantSeed = { _A=17, _B=79, _C=1111 },

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "~w starting random source with rand (~p), "
			"using default constant seed ~w.",
			[ self(), ?rand_algorithm, ConstantSeed ] ) ),

	rand:seed( ?rand_algorithm, ConstantSeed );


start_random_source( time_based_seed ) ->

	% Each run will result in different random series (erlang:now/0 was
	% previously used):
	%
	% (refer to:
	% http://erlang.org/doc/apps/erts/time_correction.html#Erlang_System_Time)
	%
	T = { A, B, C } = { erlang:monotonic_time(), erlang:unique_integer(),
						erlang:time_offset() },

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "~w forging time-based seed ~p.", [ self(), T ] ),
		basic_utils:ignore_unused( T ) ),

	% Directly inspired from third example in
	% http://osdir.com/ml/erlang-questions-programming/2013-10/msg00244.html:
	%
	start_random_source( A + erlang:phash2( C ), B, 690123 + 16384*C ).



% doc rand can be seeded.
can_be_seeded() ->
	true.



% doc: Resets the random source with a new seed.
reset_random_source( Seed ) ->
	% New seeding (stored in the process dictionary), as opposed to the setting
	% of a previously defined state:
	%
	start_random_source( Seed ).



% doc: Stops the random source.
stop_random_source() ->
	ok.



% doc: Returns a random float uniformly distributed between 0.0 (included) and
% 1.0 (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value() ->
	%random:uniform().
	rand:uniform().



% doc: Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( N ) when is_integer( N ) ->
	%random:uniform( N ).
	rand:uniform( N );

get_uniform_value( N ) ->
	throw( { not_integer, N } ).



% doc: Returns an integer random value generated from an uniform distribution in
% [Nmin,Nmax] (i.e. both bounds included), updating the random state in the
% process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( Nmin, Nmax ) when is_integer( Nmin )
					andalso is_integer( Nmax ) andalso Nmin =< Nmax ->

	% For example if Nmin = 3, Nmax = 5, we can draw value in [3, 4, 5], hence:
	%
	% N = 5 - 3 + 1 = 3.
	%
	N = Nmax - Nmin + 1,

	% Drawn in [1,N]:
	get_uniform_value( N ) + Nmin - 1;

get_uniform_value( Nmin, Nmax ) ->
	throw( { not_integer_bounds, { Nmin, Nmax } } ).



% doc: Returns a floating-point random value in [0.0,N[ generated from an
% uniform distribution.
%
% Given a number (integer or float) N (positive or not), returns a random
% floating-point value uniformly distributed between 0.0 (included) and N
% (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( N ) ->
	% Generated float in [0.0, 1.0[:
	N * rand:uniform().


% doc: Returns a floating-point random value in [Nmin, Nmax[ generated from an
% uniform distribution.
%
% Given two numbers (integer or float) Nmin and Nmax (each being positive or
% not), returns a random floating-point value uniformly distributed between Nmin
% (included) and Nmax (excluded), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( Nmin, Nmax ) ->

	%trace_utils:debug_fmt( "Generating uniform value in [~w,~w].",
	%						[ Nmin, Nmax ] ),

	% Generated float in [0.0, 1.0[:
	Nmin + ( Nmax - Nmin ) * rand:uniform().



% doc: Returns the name of the module managing the random generation.
%
% Spec already specified, for all random settings.
%
-spec get_random_module_name() -> 'rand'. %'random'.
get_random_module_name() ->
	%random.
	rand.


% doc: Returns the random state of the current process (it is useful for example
% for process serialisations).
%
% Spec already specified, for all random settings.
%
get_random_state() ->

	% Read from the process dictionary:

	% Actually, no state should not be considered as an error:
	%case erlang:get( random_seed ) of
	%
	%   undefined ->
	%       % Probably that there has been no prior seeding:
	%       throw( random_state_not_available );
	%
	%   S ->
	%       S
	%
	%end.

	% May return 'undefined', if not seeded yet:
	%erlang:get( random_seed ).
	%erlang:get( rand_seed ).
	% Best:
	rand:export_seed().



% doc: Sets the random state of this process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
set_random_state( RandomState ) ->

	% All are in the process dictionary (beware!):
	%erlang:put( random_seed, NewState ).
	%erlang:put( rand_seed, NewState ).
	rand:seed( RandomState ).


-endif. % use_crypto_module not defined






% Now sections that do not depend on defines.


% Seeding section.


% The upper bound for a seed element.
-define( seed_upper_bound, 65500 ).


% @doc Returns a seed obtained from the random source in use.
%
% This is a randomly-determined seed, meant to be used to create another random
% generator.
%
-spec get_random_seed() -> seed().
get_random_seed() ->
	{ get_uniform_value( ?seed_upper_bound ),
	  get_uniform_value( ?seed_upper_bound ),
	  get_uniform_value( ?seed_upper_bound ) }.



% @doc Checks that the specified seed is valid.
%
% For example, at least with some algorithms, {0, 0, 0} does not yield a correct
% random series.
%
-spec check_random_seed( seed() ) -> void().
check_random_seed( { A, B, C } ) when is_integer( A ) andalso is_integer( B )
		andalso is_integer( C ) andalso A > 0 andalso B > 0 andalso C > 0 ->
	ok;

check_random_seed( S ) ->
	throw( { invalid_random_seed, S } ).



% @doc Initialises the specified random law from its specification, so that as
% many samples as wanted can be drawn from its returned precomputed data
% afterwards.
%
-spec initialise_law( random_law_spec() ) -> random_law_data().
% First type of direct arbitrary spec: already with a probability distribution
% (as opposed to a PDF); easy cases:
%
initialise_law( _LS={ uniform, Max } ) ->
	CanonSpec = canonicalise_pdf_based_spec( { uniform, _Min=0, Max } ),
	{ CanonSpec, _MaybeAliasTable=undefined };

initialise_law( LS={ uniform, _Min, _Max } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, _MaybeAliasTable=undefined };


initialise_law( _LS={ integer_uniform, Max } ) ->
	CanonSpec = canonicalise_pdf_based_spec( { integer_uniform, _Min=0, Max } ),
	{ CanonSpec, _MaybeAliasTable=undefined };

initialise_law( LS={ integer_uniform, _Min, _Max } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, _MaybeAliasTable=undefined };


% Alternate name for exponential_1p:
initialise_law( LS={ exponential, _Lambda } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, _MaybeAliasTable=undefined };

initialise_law( LS={ exponential_1p, _Lambda } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, _MaybeAliasTable=undefined };

initialise_law( LS={ positive_integer_exponential_1p, _Lambda } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, _MaybeAliasTable=undefined };

% Done below, as arbitrary:
%initialise_law( LS={ exponential_2p, _Lambda, _Gamma } ) ->
%   CanonSpec = canonicalise_pdf_based_spec( LS ),
%   { CanonSpec, _MaybeAliasTable=undefined };


initialise_law( LS={ gaussian, _Mu, _Sigma } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, _MaybeAliasTable=undefined };

% Alternate name for gaussian:
initialise_law( LS={ normal_2p, _Mu, _Sigma } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, _MaybeAliasTable=undefined };

initialise_law( LS={ positive_integer_gaussian, _Mu, _Sigma } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, _MaybeAliasTable=undefined };


initialise_law( _LS={ arbitrary, AnyName, DistProbLikes } )
									when is_list( DistProbLikes ) ->

	Name = text_utils:ensure_binary( AnyName ),
	SampleCount = length( DistProbLikes ),

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "Initialising an arbitrary random law "
			"named '~ts', from a distribution comprising ~B probabilities.",
			[ Name, SampleCount ] ) ),

	PseudoSpec = { arbitrary, Name, SampleCount, _MaybeBounds=undefined },

	AliasTable = generate_alias_table_from( DistProbLikes ),

	_ArbitraryLawData={ PseudoSpec, AliasTable };


% Second type of arbitrary spec: with a PDF (with variations); no relevant
% matching possible in head due to tuples of various sizes:
%
initialise_law( LS ) ->

	case canonicalise_pdf_based_spec( LS ) of

		{ _CanSpec={ exponential_2p, Lambda, Gamma, SampleCount,
					 ExpBounds={ Min, Max } }, Inc, Exp2pPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising an Exponential-2p law "
					"of lambda=~f and gamma=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ Lambda, Gamma, math_utils:bounds_to_string( ExpBounds ),
					  SampleCount, Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( Exp2pPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			Exp2pLawSettings =
				{ exponential_2p, Lambda, Gamma, SampleCount, ExpBounds },

			_ArbitraryLawData={ Exp2pLawSettings, AliasTable };


		{ _CanSpec={ gamma_2p, K, Theta, SampleCount,
					 GamBounds={ Min, Max } }, Inc, GamPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Gamma-2p law of k=~f "
					"and theta=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ K, Theta, math_utils:bounds_to_string( GamBounds ),
					  SampleCount, Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( GamPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			GamLawSettings =
				{ gamma_2p, K, Theta, SampleCount, GamBounds },

			_ArbitraryLawData={ GamLawSettings, AliasTable };


		{ _CanSpec={ gamma_3p, Alpha, Beta, Theta, SampleCount,
					 GamBounds={ Min, Max } }, Inc, GamPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Gamma-3p law of "
					"alpha=~f, beta=~f and theta=~f, discretised on "
					"interval ~ts with ~B points (increment: ~f).",
					[ Alpha, Beta, Theta,
					  math_utils:bounds_to_string( GamBounds ), SampleCount,
					  Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( GamPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			GamLawSettings =
				{ gamma_3p, Alpha, Beta, Theta, SampleCount, GamBounds },

			_ArbitraryLawData={ GamLawSettings, AliasTable };


		{ _CanSpec={ gumbel_2p, Mu, Beta, SampleCount,
					 GumBounds={ Min, Max } }, Inc, GumPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Gumbel-2p law of mu=~f "
					"and beta=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ Mu, Beta, math_utils:bounds_to_string( GumBounds ),
					  SampleCount, Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( GumPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			GumLawSettings = { gumbel_2p, Mu, Beta, SampleCount, GumBounds },

			_ArbitraryLawData={ GumLawSettings, AliasTable };


		{ _CanSpec={ loglogistic_2p, Alpha, Beta, SampleCount,
					 LogBounds={ Min, Max } }, Inc, LogPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Log-logistic-2p law "
					"of alpha=~f and beta=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ Alpha, Beta, math_utils:bounds_to_string( LogBounds ),
					  SampleCount, Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( LogPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			LogLawSettings =
				{ loglogistic_2p, Alpha, Beta, SampleCount, LogBounds },

			_ArbitraryLawData={ LogLawSettings, AliasTable };


		{ _CanSpec={ loglogistic_3p, Alpha, Beta, Theta, SampleCount,
					 LogBounds={ Min, Max } }, Inc, LogPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Log-logistic-3p law "
					"of alpha=~f, beta=~f and theta=~f, discretised on "
					"interval ~ts with ~B points (increment: ~f).",
					[ Alpha, Beta, Theta,
					  math_utils:bounds_to_string( LogBounds ), SampleCount,
					  Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( LogPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			LogLawSettings =
				{ loglogistic_3p, Alpha, Beta, Theta, SampleCount, LogBounds },

			_ArbitraryLawData={ LogLawSettings, AliasTable };


		{ _CanSpec={ lognormal_2p, Mu, Sigma, SampleCount,
					 LogBounds={ Min, Max } }, Inc, LogPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Log-normal-2p law "
					"of mu=~f and sigma=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ Mu, Sigma, math_utils:bounds_to_string( LogBounds ),
					  SampleCount, Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( LogPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			LogLawSettings =
				{ lognormal_2p, Mu, Sigma, SampleCount, LogBounds },

			_ArbitraryLawData={ LogLawSettings, AliasTable };


		{ _CanSpec={ lognormal_3p, Mu, Sigma, Theta, SampleCount,
					 LogBounds={ Min, Max } }, Inc, LogPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Log-normal-3p law "
					"of mu=~f, sigma=~f and theta=~f, discretised on "
					"interval ~ts with ~B points (increment: ~f).",
					[ Mu, Sigma, Theta,
					  math_utils:bounds_to_string( LogBounds ), SampleCount,
					  Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( LogPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			LogLawSettings =
				{ lognormal_3p, Mu, Sigma, Theta, SampleCount, LogBounds },

			_ArbitraryLawData={ LogLawSettings, AliasTable };


		{ _CanSpec={ weibull_2p, K, Lambda, SampleCount,
					 WbBounds={ Min, Max } }, Inc, WbPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Weibull-2p law of k=~f "
					"and lambda=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ K, Lambda, math_utils:bounds_to_string( WbBounds ),
					  SampleCount, Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( WbPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			WbLawSettings = { weibull_2p, K, Lambda, SampleCount, WbBounds },

			_ArbitraryLawData={ WbLawSettings, AliasTable };


		{ _CanSpec={ weibull_3p, K, Lambda, Gamma, SampleCount,
					 WbBounds={ Min, Max } }, Inc, WbPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Weibull-3p law of k=~f, "
					"lambda=~f and gamma=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ K, Lambda, Gamma, math_utils:bounds_to_string( WbBounds ),
					  SampleCount, Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( WbPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			WbLawSettings =
				{ weibull_3p, K, Lambda, Gamma, SampleCount, WbBounds },

			_ArbitraryLawData={ WbLawSettings, AliasTable };


		{ _CanSpec={ weibull_cr, Lambda, K, Theta, SampleCount,
					 WbBounds={ Min, Max } }, Inc, WbPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Weibull-CR law "
					"of lambda=~f, k=~f and theta=~f, discretised on "
					"interval ~ts with ~B points (increment: ~f).",
					[ Lambda, K, Theta, math_utils:bounds_to_string( WbBounds ),
					  SampleCount, Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( WbPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			WbLawSettings =
				{ weibull_cr, Lambda, K, Theta, SampleCount, WbBounds },

			_ArbitraryLawData={ WbLawSettings, AliasTable };


		{ _CanSpec={ weibull_ds, Lambda, K, Sigma, SampleCount,
					 WbBounds={ Min, Max } }, Inc, WbPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Weibull-DS law "
					"of lambda=~f, k=~f and sigma=~f, discretised on "
					"interval ~ts with ~B points (increment: ~f).",
					[ Lambda, K, Sigma, math_utils:bounds_to_string( WbBounds ),
					  SampleCount, Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( WbPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			WbLawSettings =
				{ weibull_ds, Lambda, K, Sigma, SampleCount, WbBounds },

			_ArbitraryLawData={ WbLawSettings, AliasTable };


		{ _CanSpec={ weibull_dszi, Lambda, K, Sigma, Theta, SampleCount,
					 WbBounds={ Min, Max } }, Inc, WbPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Weibull-DSZI law "
					"of lambda=~f, k=~f, sigma=~f and theta=~f, discretised on "
					"interval ~ts with ~B points (increment: ~f).",
					[ Lambda, K, Sigma, Theta,
					  math_utils:bounds_to_string( WbBounds ), SampleCount,
					  Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( WbPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			WbLawSettings = { weibull_dszi, Lambda, K, Sigma, Theta,
							  SampleCount, WbBounds },

			_ArbitraryLawData={ WbLawSettings, AliasTable };


		{ _CanSpec={ weibull_mixture, P, Lambda1, K1, Lambda2, K2,
					 SampleCount, WbBounds={ Min, Max } }, Inc, WbPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Weibull-mixture law "
					"of p=~f, lambda1=~f, k1=~f, lambda2=~f and k2=~f, "
					"discretised on interval ~ts with ~B points "
					"(increment: ~f).",
					[ P, Lambda1, K1, Lambda2, K2,
					  math_utils:bounds_to_string( WbBounds ), SampleCount,
					  Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( WbPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			WbLawSettings = { weibull_mixture, P, Lambda1, K1, Lambda2, K2,
							  SampleCount, WbBounds },

			_ArbitraryLawData={ WbLawSettings, AliasTable };


		{ _CanSpec={ weibull_zi, Lambda, K, P, SampleCount,
					 WbBounds={ Min, Max } }, Inc, WbPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Weibull-ZI law "
					"of lambda=~f, k=~f and p=~f, discretised on "
					"interval ~ts with ~B points (increment: ~f).",
					[ Lambda, K, P,
					  math_utils:bounds_to_string( WbBounds ), SampleCount,
					  Inc ] ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( WbPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			WbLawSettings = { weibull_zi, Lambda, K, P, SampleCount, WbBounds },

			_ArbitraryLawData={ WbLawSettings, AliasTable };


		{ _CanSpec={ beta_2p, Alpha, Beta, SampleCount,
					 BetaBounds={ Min, Max } }, Inc, BetaPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Beta-2p law of alpha=~f "
					"and beta=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ Alpha, Beta, math_utils:bounds_to_string( BetaBounds ),
					  SampleCount, Inc ] ),
				basic_utils:ignore_unused( BetaBounds ) ),

			SampledPDFPairs =
				math_utils:sample_as_pairs( BetaPDFFun, Min, Max, Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			BetaLawSettings =
				{ beta_2p, Alpha, Beta, SampleCount, BetaBounds },

			_ArbitraryLawData={ BetaLawSettings, AliasTable };


		{ _CanSpec={ arbitrary, BinName,
			_CanonPDFInfo={ LFun, SampleCount, CanBounds={ Min, Max } } },
		  Inc } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:info_fmt( "Initialising an arbitrary random law "
					"named '~ts', whose PDF is ~p, sampled on interval ~ts "
					"with ~B points (increment: ~f).",
					[ BinName, LFun, math_utils:bounds_to_string( CanBounds ),
					  SampleCount, Inc ] ),
				basic_utils:ignore_unused( CanBounds ) ),

			SampledPDFPairs = math_utils:sample_as_pairs( LFun, Min, Max, Inc ),

			% Uniform sampling; not normalised:
			AliasTable = generate_alias_table_from( SampledPDFPairs ),

			PseudoSpec = { arbitrary, BinName, SampleCount, CanBounds },

			_ArbitraryLawData={ PseudoSpec, AliasTable };


		Other ->
			throw( { unexpected_canonicalised_spec, Other } )

	end.



% @doc Returns the settings of the law specified by its data.
-spec get_law_settings( random_law_data() ) -> random_law_settings().
get_law_settings( _LawData={ Settings, _AliasTable } ) ->
	Settings.


% @doc Returns the name of the law corresponding to the specified law settings.
-spec get_law_name( random_law_settings() ) -> law_name().
get_law_name( LawSettings ) ->
	element( _Index=1, LawSettings ).


% at-doc Returns the sampling information stored in the specified law settings.
%-spec get_sampling_info( random_law_settings() ) -> sampling_info().
%get_sampling_info( SettingsTuple ) ->
%   type_utils:get_last_tuple_element( SettingsTuple ).



% Section for the generation of random samples according to a uniform law.


% @doc Returns a boolean random value generated from a uniform distribution.
%
% Therefore true and false are equally likely to be returned.
%
-spec get_boolean() -> boolean().
get_boolean() ->
	get_uniform_value( 0, 100 ) >= 49.



% @doc Returns a random element of the specified list, selected according to a
% uniform distribution.
%
-spec one_of( [ any() ] ) -> any().
one_of( ListOfThings ) ->
	Index = get_uniform_value( length( ListOfThings ) ),
	lists:nth( Index, ListOfThings ).



% @doc Returns a list of the specified number of unique elements drawn from the
% specified input list (so that there is no duplicate in the returned list).
%
% Note: defined to ease interface look-up; one should use directly
% list_utils:draw_elements_from/2 instead.
%
-spec get_random_subset( count(), list() ) -> list().
get_random_subset( ValueCount, InputList ) ->
	list_utils:draw_elements_from( InputList, ValueCount ).





% Section for the generation of random samples according to an exponential law.
%
% Note: each of the three forms comes in two versions, with floating-point or
% (positive) integer values being returned.



% @doc Returns an exponential-1p floating-point random value, with Lambda being
% the rate parameter.
%
% As get_uniform_value/1 never returns 1.0, a strictly positive value is always
% returned.
%
% See exponential_law() for further details.
%
% Using ad-hoc inverse transform sampling here.
%
-spec get_exponential_1p_value( rate() ) -> float().
get_exponential_1p_value( Lambda ) ->
	%trace_utils:debug_fmt( "Lambda=~p", [ Lambda ] ),
	- math_utils:ln( get_uniform_value() ) / Lambda.



% @doc Returns an exponential (positive) integer random value, with Lambda being
% the rate parameter.
%
% See get_exponential_value_1p/1 for further details.
%
-spec get_positive_integer_exponential_1p_value( rate() ) -> non_neg_integer().
get_positive_integer_exponential_1p_value( Lambda ) ->
	round( get_exponential_1p_value( Lambda ) ).


% @doc Returns a list of Count exponential-1p values according to the specified
% Lambda setting.
%
% See get_exponential_value_1p/1 for further details.
%
-spec get_exponential_1p_values( rate(), sample_count() ) -> [ float() ].
get_exponential_1p_values( Lambda, Count ) ->
	generate_exponential_1p_list( Lambda, Count, _Acc=[] ).



% The generate_*_list could rely on higher-order functions.


% @doc Generates a list of Count exponential-1p random values.
%
% (helper)
-spec generate_exponential_1p_list( rate(), sample_count(), [ float() ] ) ->
										[ float() ].
generate_exponential_1p_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_exponential_1p_list( Lambda, Count, Acc ) ->
	generate_exponential_1p_list( Lambda, Count-1,
		[ get_exponential_1p_value( Lambda ) | Acc ] ).



% @doc Returns a list of Count positive integer exponential-1p values according
% to the specified Lambda setting.
%
% See get_exponential_value_1p/1 for further details.
%
-spec get_positive_integer_exponential_1p_values( rate(), sample_count() ) ->
											[ non_neg_integer() ].
get_positive_integer_exponential_1p_values( Lambda, Count ) ->
	generate_positive_integer_exponential_1p_list( Lambda, Count, _Acc=[] ).


% (helper)
generate_positive_integer_exponential_1p_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_exponential_1p_list( Lambda, Count, Acc ) ->
	generate_positive_integer_exponential_1p_list( Lambda, Count-1,
		[ get_positive_integer_exponential_1p_value( Lambda ) | Acc ] ).






% Section for the generation of random samples according to a gaussian law.



% @doc Returns a random value generated from the normal (Gaussian) distribution
% with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns a random
% floating-point value drawn according to the corresponding Gaussian law,
% updating the state in the process dictionary.
%
-spec get_gaussian_value( mean(), standard_deviation() ) -> float().
get_gaussian_value( Mu, Sigma ) ->
	sigma_loop( Mu, Sigma ).



% @doc Returns a non-negative integer random value generated from the
% normal (Gaussian) distribution with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative.
%
-spec get_positive_integer_gaussian_value( mean(), standard_deviation() ) ->
											non_neg_integer().
get_positive_integer_gaussian_value( Mu, Sigma ) ->
	sigma_loop_positive_integer( Mu, Sigma ).




% @doc Generates a new gaussian value and updates the state.
%
% Mu is the mean, Sigma is the standard deviation (variance being its square).
%
% Returns the computed value.
%
% See also
% https://en.wikipedia.org/wiki/Normal_distribution#Computational_methods
%
-spec sigma_loop( mean(), standard_deviation() ) -> float().
sigma_loop( Mu, Sigma ) ->

	% Best (most efficient) implementation that could be used in the future:
	% https://en.wikipedia.org/wiki/Ziggurat_algorithm

	% Note: at least for (Mu=0, Sigma=1), rand:normal/0 could be used.

	% Using here the second best approach, the Marsaglia polar method (see
	% https://en.wikipedia.org/wiki/Marsaglia_polar_method); used for example by
	% C++11 GNU GCC libstdc++.

	% So V1 and V2 are in [-1.0;1.0[:
	V1 = 2.0 * get_uniform_value() - 1.0,

	% Supposedly independent from V1:
	V2 = 2.0 * get_uniform_value() - 1.0,

	S  = (V1 * V1) + (V2 * V2),

	% Loop until S in ]0,1[:
	case S >= 1.0 orelse S == 0.0 of

		% Rejected:
		true ->
			sigma_loop( Mu, Sigma );

		_False ->

			% Here S in ]0;1.0[ (note that 1.0 should be included, possibly by
			% transforming any 0.0 in a 1.0):
			%
			%trace_utils:debug_fmt( "Mu = ~p, Sigma = ~p, S = ~p.",
			%                       [ Mu, Sigma, S ] ),

			% math:log/1 is the Natural Log (base e log):
			Scale = sqrt( ( -2.0 * ln( S ) ) / S ),

			% Adjust for standard deviation and mean:
			Mu + Sigma * Scale * V1

	end.



% @doc Generates a new integer non-negative gaussian value and updates the
% state.
%
% Returns the computed value.
%
-spec sigma_loop_positive_integer( mean(), standard_deviation() ) ->
												non_neg_integer().
sigma_loop_positive_integer( Mu, Sigma ) ->

	% Loops until a positive integer is found:
	case round( sigma_loop( Mu, Sigma ) ) of

		TriedValue when TriedValue < 0 ->
			sigma_loop_positive_integer( Mu, Sigma );

		NonNegativeValue ->
			NonNegativeValue

	end.



% @doc Returns a list of Count Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random floating-point
% values drawn according the corresponding Gaussian law, updating the state in
% the process dictionary.
%
-spec get_gaussian_values( mean(), standard_deviation(), sample_count() ) ->
													[ float() ].
get_gaussian_values( Mu, Sigma, Count ) ->
	generate_gaussian_list( Mu, Sigma, Count, _Acc=[] ).



% @doc Generates a list of Count Gaussian random values.
%
% (helper)
generate_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_gaussian_list( Mu, Sigma, Count-1,
							[ sigma_loop( Mu, Sigma ) | Acc ] ).



% @doc Returns a list of Count positive integer Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
-spec get_positive_integer_gaussian_values( mean(), standard_deviation(),
						sample_count() ) -> [ non_neg_integer() ].
get_positive_integer_gaussian_values( Mu, Sigma, Count ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count ).



% @doc Generates a list of Count positive integer Gaussian random values.
-spec generate_positive_integer_gaussian_list( mean(), standard_deviation(),
			sample_count() ) -> [ non_neg_integer() ].
generate_positive_integer_gaussian_list( Mu, Sigma, Count ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count, [] ).


% (helper)
generate_positive_integer_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count-1,
		[ erlang:round( sigma_loop_positive_integer( Mu, Sigma ) ) | Acc ] ).





% Section for the generation of random samples, including from arbitrary,
% non-uniform distributions.
%
% Drawing from user-specified random laws (probability distributions / PDFs,
% i.e. Probability Density Functions) corresponds to operating inverse transform
% sampling (see https://en.wikipedia.org/wiki/Inverse_transform_sampling).

% Here, such sampling is done on discrete (as opposed to continuous)
% probabilities, which can be done efficiently thanks to the alias method (see
% https://en.wikipedia.org/wiki/Alias_method), also known as the Walker method.
%
% The current implementation derives directly notably from the one kindly shared
% (Apache License Version 2.0) by Sergey Prokhorov (email: me at seriyps dot
% ru), refer to https://gist.github.com/seriyps/5593193, which itself derived
% from a Python implementation.



% @doc Returns a precomputed alias table used in order to produce samples
% according to the specified discrete probability distribution.
%
% These samples do not have to be normalised first.
%
% For example MyDistribAliasTable = random_utils:generate_alias_table_from(
%   [{a,10}, {b,20}, {c,40}, {d,30}]).
%
% or MyDistribAliasTable = random_utils:generate_alias_table_from(
%   [{"hello",0.6}, {"goodbye",0.4}]).
%
% From this (const) table, computed once for all and whose construction does not
% depend on any other random state (it is deterministic), any number of samples
% respecting said distribution can be drawn.
%
% This preprocessing uses O(N) time, where N is the number of declared samples
% of the corresponding distribution, in order to generate its returned table.
%
-spec generate_alias_table_from( discrete_probability_distribution() ) ->
											alias_table().
generate_alias_table_from( DiscreteProbDist ) ->
	{ SampleValues, ProbLikes } = lists:unzip( DiscreteProbDist ),
	generate_alias_table_from( SampleValues, ProbLikes ).


% (helper)
-spec generate_alias_table_from( [ sample() ], [ probability_like() ] ) ->
										alias_table().
generate_alias_table_from( SampleValues, ProbLikes ) ->

	EntryCount = length( ProbLikes ),

	% Notations from https://en.wikipedia.org/wiki/Alias_method#Table_generation

	% Ui = n.pi:

	% Sum expected to be non-null (at least one non-null probability requested):
	Factor = EntryCount / lists:sum( ProbLikes ),

	% Normalised:
	ScaledProbs = [ Factor * PL || PL <- ProbLikes ],

	{ UnderFulls, OverFulls } = split_by_index_fullness( ScaledProbs ),

	%trace_utils:debug_fmt( "EntryCount = ~p, UnderFulls = ~p, OverFulls = ~p",
	%                       [ EntryCount, UnderFulls, OverFulls ] ),

	{ Indexes, UpdatedProbLikes } = fill_entries( UnderFulls, OverFulls,
		array:new( EntryCount ), array:from_list( ScaledProbs ) ),

	%trace_utils:debug_fmt( "Sample value array: ~p~nIndex array: ~p~n"
	%   "Prob-like array: ~p",
	%   [ SampleValues, array:to_list( Indexes ),
	%     array:to_list( UpdatedProbLikes ) ] ),

	SampleArray = array:from_list( SampleValues ),

	#alias_table{ entry_count=EntryCount,
				  sample_values=SampleArray,
				  indexes=Indexes,
				  prob_likes=UpdatedProbLikes }.



% Returns two index lists, the second corresponding to the "overfull" group
% (where Ui > 1), the first to the "underfull" group (where (Ui < 1 and Ki has
% not been initialised) or (where Ui = 1 or Ki has been initialised) - that is,
% respectively, the "strict underfull" subgroup and the "exactly full" one.
%
% (helper)
%
-spec split_by_index_fullness( [ probability_like() ] ) ->
					{ UnderFulls :: [ positive_index() ],
					  OverFulls  :: [ positive_index() ]  }.
split_by_index_fullness( ScaledProbLikes ) ->
	split_by_index_fullness( ScaledProbLikes, _UnderFulls=[],
							 _OverFulls=[], _Idx=0 ).


% (helper)
split_by_index_fullness( _ScaledProbLikes=[], UnderFulls, OverFulls, _Idx ) ->
	{ UnderFulls, OverFulls };

split_by_index_fullness( _ScaledProbLikes=[ PL | T ], UnderFulls, OverFulls,
						 Idx ) when PL < 1 ->
	split_by_index_fullness( T, [ Idx | UnderFulls ], OverFulls, Idx+1 );

split_by_index_fullness( _ScaledProbLikes=[ _PL | T ], UnderFulls, OverFulls,
						 Idx ) -> % Implicit: when PL >= 1 ->
	split_by_index_fullness( T,  UnderFulls, [ Idx | OverFulls ], Idx+1 ).



% Allocates the unused space in an underfull entry (U) to an overfull one (O),
% so that U becomes exactly full.
%
% (helper)
%
fill_entries( _UnderFulls=[ UIdx | TU ], _OverFulls=[ OIdx | TO ], IdxArray,
			  ProbLikeArray ) ->
	% Allocate the unused space in entry O to outcome U:
	NewIdxArray = array:set( _Idx=UIdx, _Value=OIdx, IdxArray ),
	UPL = array:get( UIdx, ProbLikeArray ),
	OPL = array:get( OIdx, ProbLikeArray ),

	% Remove the allocated space from entry U:
	NewOPL = OPL + UPL - 1,

	NewProbLikeArray = array:set( OIdx, NewOPL, ProbLikeArray ),

	% Place O in the right list:
	{ NewUnderFulls, NewOverFulls } = case NewOPL < 1 of

		true ->
			{ [ OIdx | TU ], TO };

		_ ->
			{ TU, [ OIdx | TO ] }

	end,

	fill_entries( NewUnderFulls, NewOverFulls, NewIdxArray, NewProbLikeArray );


% Implicitly UnderFulls and/or OverFulls is empty here:
fill_entries( _UnderFulls, _OverFulls, IdxArray, ProbLikeArray ) ->
	{ IdxArray, ProbLikeArray }.



% @doc Returns, for the specified law specification, all the sample pairs that
% correspond to it, along with extraneous information.
%
% Useful for testing, knowing that in the general case the input samples are not
% kept in the initialised random law.
%
-spec get_all_sample_pairs( random_law_spec() ) ->
									discrete_probability_distribution().
% Starting with the non-PDF law specs:
get_all_sample_pairs( _LS={ uniform, Min, Max } ) ->
	% Constant (non-normalised) probability:
	LawFun = fun( _S ) -> 1.0 / ?default_pdf_sample_count end,

	% Any increment would do:
	Inc = ( Max - Min ) / ?default_pdf_sample_count,

	% Returns SampledPDFPairs (uniform sampling; not normalised):
	math_utils:sample_as_pairs( LawFun, Min, Max, Inc );

get_all_sample_pairs( _LS={ integer_uniform, Min, Max } ) ->
	% Constant (non-normalised) probability:
	LawFun =
		fun( S ) ->

			case math_utils:are_relatively_close( S, round( S ) ) of

				true ->
					1.0;

				false ->
					0.0

			end

		end,

	% Returns SampledPDFPairs (uniform integer sampling; not normalised):
	math_utils:sample_as_pairs( LawFun, Min, Max, _Inc=1 );


get_all_sample_pairs( _LS={ exponential, Lambda } ) ->
	get_all_sample_pairs( { exponential_1p, Lambda } );


get_all_sample_pairs( _LS={ exponential_1p, Lambda } ) ->

	LawFun = fun( S ) -> exponential_1p_pdf( S, Lambda ) end,

	{ Min, Max } = math_utils:compute_support( LawFun ),

	Inc = ( Max - Min ) / ?default_pdf_sample_count,

	math_utils:sample_as_pairs( LawFun, Min, Max, Inc );


get_all_sample_pairs( _LS={ positive_integer_exponential_1p, Lambda } ) ->

	LawFun = fun( S ) ->

			case math_utils:are_relatively_close( S, round( S ) ) of

				true ->
					exponential_1p_pdf( S, Lambda );

				false ->
					0.0

			end

		end,

	{ Min, Max } = math_utils:compute_integer_support( LawFun ),

	% Integer distribution:
	math_utils:sample_as_pairs( LawFun, Min, Max, _Inc=1 );


get_all_sample_pairs( _LS={ exponential_2p, Lambda, Gamma } ) ->

	LawFun = fun( S ) -> exponential_2p_pdf( S, Lambda, Gamma ) end,

	{ Min, Max } = math_utils:compute_support( LawFun ),

	Inc = ( Max - Min ) / ?default_pdf_sample_count,

	math_utils:sample_as_pairs( LawFun, Min, Max, Inc );


get_all_sample_pairs( _LS={ gaussian, Mu, Sigma } ) ->

	LawFun = fun( S ) -> gaussian_pdf( S, Mu, Sigma ) end,

	{ Min, Max } = math_utils:compute_support( LawFun ),

	Inc = ( Max - Min ) / ?default_pdf_sample_count,

	math_utils:sample_as_pairs( LawFun, Min, Max, Inc );


get_all_sample_pairs( _LS={ positive_integer_gaussian, Mu, Sigma  } ) ->

	LawFun = fun( S ) ->

			case math_utils:are_relatively_close( S, round( S ) ) of

				true ->
					gaussian_pdf( S, Mu, Sigma );

				false ->
					0.0

			end

		end,

	{ Min, Max } = math_utils:compute_integer_support( LawFun, _Origin=Mu,
		_MaybeMin=undefined, _MaybeMax=undefined ),

	% Integer distribution:
	math_utils:sample_as_pairs( LawFun, Min, Max, _Inc=1 );



% First type of arbitrary spec: already with a probability distribution (as
% opposed to a PDF).
%
get_all_sample_pairs( _LS={ arbitrary, _AnyName, DistProbLikes } )
								when is_list( DistProbLikes ) ->
	% The easy case:
	DistProbLikes;

% The last category is the PDF-based (no relevant matching possible in head, LS
% tuple tag checked by canonicalise_arbitrary_pdf_spec/1):
%
get_all_sample_pairs( LS ) ->

	{ LawFun, CanonBounds, Increment } =
			case canonicalise_pdf_based_spec( LS ) of


		{ _CanonSpec={ gamma_2p, _K, _Theta, _SampleCount, GamBounds }, Inc,
		  GamPDFFun } ->
			{ GamPDFFun, GamBounds, Inc };

		{ _CanonSpec={ gamma_3p, _Alpha, _Beta, _Theta, _SampleCount,
					   GamBounds }, Inc, GamPDFFun } ->
			{ GamPDFFun, GamBounds, Inc };


		{ _CanonSpec={ gumbel_2p, _Mu, _Beta, _SampleCount, GumBounds }, Inc,
		  GumPDFFun } ->
			{ GumPDFFun, GumBounds, Inc };


		{ _CanonSpec={ loglogistic_2p, _Alpha, _Beta, _SampleCount, LogBounds },
		  Inc, LogPDFFun } ->
			{ LogPDFFun, LogBounds, Inc };

		{ _CanonSpec={ loglogistic_3p, _Alpha, _Beta, _Theta, _SampleCount,
					   LogBounds }, Inc, LogPDFFun } ->
			{ LogPDFFun, LogBounds, Inc };


		{ _CanonSpec={ lognormal_2p, _Mu, _Sigma, _SampleCount, LogBounds },
		  Inc, LogPDFFun } ->
			{ LogPDFFun, LogBounds, Inc };

		{ _CanonSpec={ lognormal_3p, _Mu, _Sigma, _Theta, _SampleCount,
					   LogBounds }, Inc, LogPDFFun } ->
			{ LogPDFFun, LogBounds, Inc };


		{ _CanonSpec={ weibull_2p, _K, _Lambda, _SampleCount, WbBounds }, Inc,
		  WbPDFFun } ->
			{ WbPDFFun, WbBounds, Inc };

		{ _CanonSpec={ weibull_3p, _K, _Lambda, _Gamma, _SampleCount,
					   WbBounds }, Inc, WbPDFFun } ->
			{ WbPDFFun, WbBounds, Inc };

		{ _CanonSpec={ weibull_cr, _Lambda, _K, _Theta, _SampleCount,
					   WbBounds }, Inc, WbPDFFun } ->
			{ WbPDFFun, WbBounds, Inc };

		{ _CanonSpec={ weibull_ds, _Lambda, _K, _Sigma, _SampleCount,
					   WbBounds }, Inc, WbPDFFun } ->
			{ WbPDFFun, WbBounds, Inc };

		{ _CanonSpec={ weibull_dszi, _Lambda, _K, _Sigma, _Theta, _SampleCount,
					   WbBounds }, Inc, WbPDFFun } ->
			{ WbPDFFun, WbBounds, Inc };

		{ _CanonSpec={ weibull_mixture, _P, _Lambda1, _K1, _Lambda2, _K2,
					   _SampleCount, WbBounds }, Inc,
		  WbPDFFun } ->
			{ WbPDFFun, WbBounds, Inc };

		{ _CanonSpec={ weibull_zi, _Lambda, _K, _P, _SampleCount, WbBounds },
		  Inc, WbPDFFun } ->
			{ WbPDFFun, WbBounds, Inc };

		{ _CanonSpec={ beta_2p, _Alpha, _Beta, _SampleCount, BetaBounds }, Inc,
		  BetaPDFFun } ->
			{ BetaPDFFun, BetaBounds, Inc };


		{ _CanonSpec={ arbitrary, _BinName,
				_CanonPDFInfo={ LFun, _SampleCount, CanBounds } }, Inc } ->
			{ LFun, CanBounds, Inc }

	end,

	% Returns SampledPDFPairs (uniform sampling; not normalised):
	math_utils:sample_as_pairs( LawFun, _Min=pair:first( CanonBounds ),
		_Max=pair:second( CanonBounds ), Increment ).




% @doc Returns canonical settings for the specified PDF-based specification.
%
% Logic centralised, as fully common to initialise_law/1 and
% get_all_sample_pairs/1, so that they use exactly the same results.
%
-spec canonicalise_pdf_based_spec( random_law_spec() ) ->

	  full_uniform_law_spec() | integer_uniform_law_spec()

  |   exponential_1p_law_spec() | exponential_law_spec() 
  | positive_integer_exponential_1p_law_spec()
  | { full_exponential_2p_law_spec(), increment(), exponential_2p_pdf() }

  | { full_gamma_2p_law_spec(), increment(), gamma_2p_pdf() }
  | { full_gamma_3p_law_spec(), increment(), gamma_3p_pdf() }

  | { full_gumbel_2p_law_spec(), increment(), gumbel_pdf() }

  | { full_loglogistic_2p_law_spec(), increment(), loglogistic_2p_pdf() }
  | { full_loglogistic_3p_law_spec(), increment(), loglogistic_3p_pdf() }

  | { full_lognormal_2p_law_spec(), increment(), lognormal_2p_pdf() }
  | { full_lognormal_3p_law_spec(), increment(), lognormal_3p_pdf() }

  | { full_weibull_law_spec(), increment(), weibull_pdf() }

  | { full_beta_law_spec(), increment(), beta_pdf() }

  | { arbitrary_law_spec(), pdf(),
		{ sample_count(), bounds(), increment() } }.
% First, uniform laws:
canonicalise_pdf_based_spec( _LS={ uniform, Min, Max } ) ->
	{ Minf, Maxf } = math_utils:canonicalise_bounds( { Min, Max } ),
	_UnifLawSpec={ uniform, Minf, Maxf };

canonicalise_pdf_based_spec( _LS={ integer_uniform, Min, Max } ) ->
	{ Mini, Maxi } = math_utils:canonicalise_integer_bounds( { Min, Max } ),
	_UnifLawSpec={ integer_uniform, Mini, Maxi };


% Then exponential laws:
canonicalise_pdf_based_spec( _LS={ exponential_1p, Lambda } ) ->
	Lambdaf = check_exponential_lambda( Lambda ),
	_ExpLawSpec={ exponential_1p, Lambdaf };

% Alias for exponential_1p:
canonicalise_pdf_based_spec( _LS={ exponential, Lambda } ) ->
	Lambdaf = check_exponential_lambda( Lambda ),
	_ExpLawSpec={ exponential_1p, Lambdaf };

canonicalise_pdf_based_spec(
		_LS={ positive_integer_exponential_1p, Lambda } ) ->
	Lambdaf = check_exponential_lambda( Lambda ),
	_ExpLawSpec={ positive_integer_exponential_1p, Lambdaf };

% Exponential-2p with no sample count:
canonicalise_pdf_based_spec( _LS={ exponential_2p, Lambda, Gamma } ) ->
	canonicalise_pdf_based_spec(
		{ exponential_2p, Lambda, Gamma, ?default_pdf_sample_count } );


% No support specified:
canonicalise_pdf_based_spec(
		LS={ exponential_2p, Lambda, Gamma, SampleCount } ) ->

	% Needed to compute support (no a priori bounds):
	{ ExpPDFFun, Lambdaf, Gammaf } =
		get_exponential_2p_pdf( Lambda, Gamma, LS ),

	SampleBounds = math_utils:compute_support( ExpPDFFun ),

	% To be shared with next clause to re-use the obtained fun:
	canonicalise_exponential_spec_with( ExpPDFFun,
		{ exponential_2p, Lambdaf, Gammaf, SampleCount, SampleBounds } );


% Full information available for 2p:
canonicalise_pdf_based_spec(
		LS={ exponential_2p, Lambda, Gamma, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	% Checks Lambda and Gamma:
	{ ExpPDFFun, Lambdaf, Gammaf } =
		get_exponential_2p_pdf( Lambda, Gamma, LS ),

	canonicalise_exponential_spec_with( ExpPDFFun,
		{ exponential_2p, Lambdaf, Gammaf, SampleCount, CanonBounds } );



% Now, the Gamma section:
%
% Gamma-2p with no sample count:
canonicalise_pdf_based_spec( _LS={ gamma_2p, K, Theta } ) ->
	canonicalise_pdf_based_spec(
		{ gamma_2p, K, Theta, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec( LS={ gamma_2p, K, Theta, SampleCount } ) ->
	% Needed to compute support (no a priori bounds):
	{ GamPDFFun, Kf, Thetaf } = get_gamma_2p_pdf( K, Theta, LS ),
	SampleBounds = math_utils:compute_support( GamPDFFun ),

	% To be shared with next clause to re-use the obtained fun:
	canonicalise_gamma_spec_with( GamPDFFun,
		{ gamma_2p, Kf, Thetaf, SampleCount, SampleBounds } );

% Full information available for 2p:
canonicalise_pdf_based_spec(
		LS={ gamma_2p, K, Theta, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	% Checks K and Theta:
	{ GamPDFFun, Kf, Thetaf } = get_gamma_2p_pdf( K, Theta, LS ),

	canonicalise_gamma_spec_with( GamPDFFun,
		{ gamma_2p, Kf, Thetaf, SampleCount, CanonBounds } );


% Gamma-3p with no sample count:
canonicalise_pdf_based_spec( _LS={ gamma_3p, Alpha, Beta, Theta } ) ->
	canonicalise_pdf_based_spec(
		{ gamma_3p, Alpha, Beta, Theta, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ gamma_3p, Alpha, Beta, Theta, SampleCount } ) ->

	% Needed to compute support:
	{ GamFun, Alphaf, Betaf, Thetaf } =
		get_gamma_3p_pdf( Alpha, Beta, Theta, LS ),

	SampleBounds = math_utils:compute_support( GamFun ),

	% To be shared with next clause, fun created once:
	canonicalise_gamma_spec_with( GamFun,
		{ gamma_3p, Alphaf, Betaf, Thetaf, SampleCount, SampleBounds } );

% Full information available for 3p:
canonicalise_pdf_based_spec(
		LS={ gamma_3p, Alpha, Beta, Theta, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	{ GamFun, Alphaf, Betaf, Thetaf } =
		get_gamma_3p_pdf( Alpha, Beta, Theta, LS ),

	canonicalise_gamma_spec_with( GamFun,
		{ gamma_3p, Alphaf, Betaf, Thetaf, SampleCount, CanonBounds } );



% Gumbel-2p with no sample count:
canonicalise_pdf_based_spec( _LS={ gumbel_2p, Mu, Beta } ) ->
	canonicalise_pdf_based_spec(
		{ gumbel_2p, Mu, Beta, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec( LS={ gumbel_2p, Mu, Beta, SampleCount } ) ->
	% Needed to compute support (no a priori bounds):
	{ GumPDFFun, Muf, Betaf } = get_gumbel_2p_pdf( Mu, Beta, LS ),
	SampleBounds = math_utils:compute_support( GumPDFFun ),

	% To be shared with next clause to re-use the obtained fun:
	canonicalise_gumbel_spec_with( GumPDFFun,
		{ gumbel_2p, Muf, Betaf, SampleCount, SampleBounds } );

% Full information available for 2p:
canonicalise_pdf_based_spec(
		LS={ gumbel_2p, Mu, Beta, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	% Checks Mu and Beta:
	{ GumPDFFun, Muf, Betaf } = get_gumbel_2p_pdf( Mu, Beta, LS ),

	canonicalise_gumbel_spec_with( GumPDFFun,
		{ gumbel_2p, Muf, Betaf, SampleCount, CanonBounds } );



% Now, the Log-logistic section:
%
% Loglogistic-2p with no sample count:
canonicalise_pdf_based_spec( _LS={ loglogistic_2p, Alpha, Beta } ) ->
	canonicalise_pdf_based_spec(
		{ loglogistic_2p, Alpha, Beta, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ loglogistic_2p, Alpha, Beta, SampleCount } ) ->
	% Needed to compute support (no a priori bounds):
	{ LogPDFFun, Alphaf, Betaf } = get_loglogistic_2p_pdf( Alpha, Beta, LS ),
	SampleBounds = math_utils:compute_support( LogPDFFun ),

	% To be shared with next clause to re-use the obtained fun:
	canonicalise_loglogistic_spec_with( LogPDFFun,
		{ loglogistic_2p, Alphaf, Betaf, SampleCount, SampleBounds } );

% Full information available for 2p:
canonicalise_pdf_based_spec(
		LS={ loglogistic_2p, Alpha, Beta, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	% Checks Alpha and Beta:
	{ LogPDFFun, Alphaf, Betaf } = get_loglogistic_2p_pdf( Alpha, Beta, LS ),

	canonicalise_loglogistic_spec_with( LogPDFFun,
		{ loglogistic_2p, Alphaf, Betaf, SampleCount, CanonBounds } );


% Loglogistic-3p with no sample count:
canonicalise_pdf_based_spec( _LS={ loglogistic_3p, Alpha, Beta, Theta } ) ->
	canonicalise_pdf_based_spec(
		{ loglogistic_3p, Alpha, Beta, Theta, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ loglogistic_3p, Alpha, Beta, Theta, SampleCount } ) ->

	% Needed to compute support:
	{ LogFun, Alphaf, Betaf, Thetaf } =
		get_loglogistic_3p_pdf( Alpha, Beta, Theta, LS ),

	SampleBounds = math_utils:compute_support( LogFun ),

	% To be shared with next clause, fun created once:
	canonicalise_loglogistic_spec_with( LogFun,
		{ loglogistic_3p, Alphaf, Betaf, Thetaf, SampleCount, SampleBounds } );


% Full information available for 3p:
canonicalise_pdf_based_spec( LS={ loglogistic_3p, Alpha, Beta, Theta,
								  SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	{ LogFun, Alphaf, Betaf, Thetaf } =
		get_loglogistic_3p_pdf( Alpha, Beta, Theta, LS ),

	canonicalise_loglogistic_spec_with( LogFun,
		{ loglogistic_3p, Alphaf, Betaf, Thetaf, SampleCount, CanonBounds } );



% Now, the Log-normal section:
%
% Lognormal-2p with no sample count:
canonicalise_pdf_based_spec( _LS={ lognormal_2p, Mu, Sigma } ) ->
	canonicalise_pdf_based_spec(
		{ lognormal_2p, Mu, Sigma, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ lognormal_2p, Mu, Sigma, SampleCount } ) ->
	% Needed to compute support (no a priori bounds):
	{ LogPDFFun, Muf, Sigmaf } = get_lognormal_2p_pdf( Mu, Sigma, LS ),
	SampleBounds = math_utils:compute_support( LogPDFFun ),

	% To be shared with next clause to re-use the obtained fun:
	canonicalise_lognormal_spec_with( LogPDFFun,
		{ lognormal_2p, Muf, Sigmaf, SampleCount, SampleBounds } );

% Full information available for 2p:
canonicalise_pdf_based_spec(
		LS={ lognormal_2p, Mu, Sigma, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	% Checks Mu and Sigma:
	{ LogPDFFun, Muf, Sigmaf } = get_lognormal_2p_pdf( Mu, Sigma, LS ),

	canonicalise_lognormal_spec_with( LogPDFFun,
		{ lognormal_2p, Muf, Sigmaf, SampleCount, CanonBounds } );


% Lognormal-3p with no sample count:
canonicalise_pdf_based_spec( _LS={ lognormal_3p, Mu, Sigma, Theta } ) ->
	canonicalise_pdf_based_spec(
		{ lognormal_3p, Mu, Sigma, Theta, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ lognormal_3p, Mu, Sigma, Theta, SampleCount } ) ->

	% Needed to compute support:
	{ LogFun, Muf, Sigmaf, Thetaf } =
		get_lognormal_3p_pdf( Mu, Sigma, Theta, LS ),

	SampleBounds = math_utils:compute_support( LogFun ),

	% To be shared with next clause, fun created once:
	canonicalise_lognormal_spec_with( LogFun,
		{ lognormal_3p, Muf, Sigmaf, Thetaf, SampleCount, SampleBounds } );


% Full information available for 3p:
canonicalise_pdf_based_spec( LS={ lognormal_3p, Mu, Sigma, Theta,
								  SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	{ LogFun, Muf, Sigmaf, Thetaf } =
		get_lognormal_3p_pdf( Mu, Sigma, Theta, LS ),

	canonicalise_lognormal_spec_with( LogFun,
		{ lognormal_3p, Muf, Sigmaf, Thetaf, SampleCount, CanonBounds } );


% Then Gaussian laws:
canonicalise_pdf_based_spec( _LS={ gaussian, Mu, Sigma } ) ->
	Muf = check_gaussian_mu( Mu ),
	Sigmaf = check_gaussian_sigma( Sigma ),
	_GausLawSpec={ normal_2p, Muf, Sigmaf };

% Synonym for normal_2p:
canonicalise_pdf_based_spec( _LS={ normal_2p, Mu, Sigma } ) ->
	Muf = check_gaussian_mu( Mu ),
	Sigmaf = check_gaussian_sigma( Sigma ),
	_GausLawSpec={ normal_2p, Muf, Sigmaf };

canonicalise_pdf_based_spec( _LS={ positive_integer_gaussian, Mu, Sigma } ) ->
	Muf = check_gaussian_mu( Mu ),
	Sigmaf = check_gaussian_sigma( Sigma ),
	_GausLawSpec={ positive_integer_gaussian, Muf, Sigmaf };



% Now, the Weibull section:
%
% Weibull-2p with no sample count:
canonicalise_pdf_based_spec( _LS={ weibull_2p, K, Lambda } ) ->
	canonicalise_pdf_based_spec(
		{ weibull_2p, K, Lambda, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec( LS={ weibull_2p, K, Lambda, SampleCount } ) ->
	% Needed to compute support (no a priori bounds):
	{ WbPDFFun, Kf, Lambdaf } = get_weibull_2p_pdf( K, Lambda, LS ),
	SampleBounds = math_utils:compute_support( WbPDFFun ),

	% To be shared with next clause to re-use the obtained fun:
	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_2p, Kf, Lambdaf, SampleCount, SampleBounds } );

% Full information available for 2p:
canonicalise_pdf_based_spec(
		LS={ weibull_2p, K, Lambda, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	% Checks K and Lambda:
	{ WbPDFFun, Kf, Lambdaf } = get_weibull_2p_pdf( K, Lambda, LS ),

	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_2p, Kf, Lambdaf, SampleCount, CanonBounds } );


% Weibull-3p with no sample count:
canonicalise_pdf_based_spec( _LS={ weibull_3p, K, Lambda, Gamma } ) ->
	canonicalise_pdf_based_spec(
		{ weibull_3p, K, Lambda, Gamma, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ weibull_3p, K, Lambda, Gamma, SampleCount } ) ->

	% Needed to compute support:
	{ WbPDFFun, Kf, Lambdaf, Gammaf } =
		get_weibull_3p_pdf( K, Lambda, Gamma, LS ),

	% As not defined (negative number exponentiated) below Gammaf:
	_SampleBounds = { SMin, SMax } = math_utils:compute_support( WbPDFFun,
		_MinBound=Gammaf, _MaxBound=undefined ),

	% As apparently samples shall be greater than Gamma (see
	% https://reliawiki.org/index.php/The_Weibull_Distribution):
	%
	Min = max( SMin, Gammaf ),

	BestBounds = { Min, SMax },

	% To be shared with next clause, fun created once:
	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_3p, Kf, Lambdaf, Gammaf, SampleCount, BestBounds } );

% Full information available for 3p:
canonicalise_pdf_based_spec(
		LS={ weibull_3p, K, Lambda, Gamma, SampleCount, SampleBounds } ) ->

	CanonBounds = { SMin, _SMax } =
		math_utils:canonicalise_bounds( SampleBounds ),

	SMin >= Gamma orelse throw( { too_small_lower_bound, SMin, Gamma } ),

	{ WbPDFFun, Kf, Lambdaf, Gammaf } =
		get_weibull_3p_pdf( K, Lambda, Gamma, LS ),

	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_3p, Kf, Lambdaf, Gammaf, SampleCount, CanonBounds } );



% Weibull-CR with no sample count:
canonicalise_pdf_based_spec( _LS={ weibull_cr, Lambda, K, Theta } ) ->
	canonicalise_pdf_based_spec(
		{ weibull_cr, Lambda, K, Theta, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ weibull_cr, Lambda, K, Theta, SampleCount } ) ->

	% Needed to compute support:
	{ WbPDFFun, Lambdaf, Kf, Thetaf } =
		get_weibull_cr_pdf( Lambda, K, Theta, LS ),

	SampleBounds = math_utils:compute_support( WbPDFFun ),

	% To be shared with next clause, fun created once:
	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_cr, Lambdaf, Kf, Thetaf, SampleCount, SampleBounds } );

% Full information available for CR:
canonicalise_pdf_based_spec(
		LS={ weibull_cr, Lambda, K, Theta, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	{ WbPDFFun, Lambdaf, Kf, Thetaf } =
		get_weibull_cr_pdf( Lambda, K, Theta, LS ),

	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_cr, Lambdaf, Kf, Thetaf, SampleCount, CanonBounds } );


% Weibull-DS with no sample count:
canonicalise_pdf_based_spec( _LS={ weibull_ds, Lambda, K, Sigma } ) ->
	canonicalise_pdf_based_spec(
		{ weibull_ds, Lambda, K, Sigma, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ weibull_ds, Lambda, K, Sigma, SampleCount } ) ->

	% Needed to compute support:
	{ WbPDFFun, Lambdaf, Kf, Sigmaf } =
		get_weibull_ds_pdf( Lambda, K, Sigma, LS ),

	SampleBounds = math_utils:compute_support( WbPDFFun ),

	% To be shared with next clause, fun dseated once:
	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_ds, Lambdaf, Kf, Sigmaf, SampleCount, SampleBounds } );

% Full information available for DS:
canonicalise_pdf_based_spec(
		LS={ weibull_ds, Lambda, K, Sigma, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	{ WbPDFFun, Lambdaf, Kf, Sigmaf } =
		get_weibull_ds_pdf( Lambda, K, Sigma, LS ),

	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_ds, Lambdaf, Kf, Sigmaf, SampleCount, CanonBounds } );


% Weibull-DSZI with no sample count:
canonicalise_pdf_based_spec( _LS={ weibull_dszi, Lambda, K, Sigma, Theta } ) ->
	canonicalise_pdf_based_spec(
		{ weibull_dszi, Lambda, K, Sigma, Theta, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ weibull_dszi, Lambda, K, Sigma, Theta, SampleCount } ) ->

	% Needed to compute support:
	{ WbPDFFun, Lambdaf, Kf, Sigmaf, Thetaf } =
		get_weibull_dszi_pdf( Lambda, K, Sigma, Theta, LS ),

	SampleBounds = math_utils:compute_support( WbPDFFun ),

	% To be shared with next clause, fun dszieated once:
	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_dszi, Lambdaf, Kf, Sigmaf, Thetaf, SampleCount,
		  SampleBounds } );

% Full information available for DSZI:
canonicalise_pdf_based_spec(
		LS={ weibull_dszi, Lambda, K, Sigma, Theta, SampleCount,
			 SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	{ WbPDFFun, Lambdaf, Kf, Sigmaf, Thetaf } =
		get_weibull_dszi_pdf( Lambda, K, Sigma, Theta, LS ),

	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_dszi, Lambdaf, Kf, Sigmaf, Thetaf, SampleCount,
		  CanonBounds } );


% Weibull-Mixture with no sample count:
canonicalise_pdf_based_spec(
		_LS={ weibull_mixture, P, Lambda1, K1, Lambda2, K2 } ) ->
	canonicalise_pdf_based_spec( { weibull_mixture, P, Lambda1, K1,
								   Lambda2, K2, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ weibull_mixture, P, Lambda1, K1, Lambda2, K2, SampleCount } ) ->

	% Needed to compute support:
	{ WbPDFFun, Pf, Lambda1f, K1f, Lambda2f, K2f } =
		get_weibull_mixture_pdf( P, Lambda1, K1, Lambda2, K2, LS ),

	SampleBounds = math_utils:compute_support( WbPDFFun ),

	% To be shared with next clause, fun mixtureeated once:
	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_mixture, Pf, Lambda1f, K1f, Lambda2f, K2f, SampleCount,
		  SampleBounds } );

% Full information available for Mixture:
canonicalise_pdf_based_spec(
		LS={ weibull_mixture, P, Lambda1, K1, Lambda2, K2, SampleCount,
			 SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	{ WbPDFFun, Pf, Lambda1f, K1f, Lambda2f, K2f } =
		get_weibull_mixture_pdf( P, Lambda1, K1, Lambda2, K2, LS ),

	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_mixture, Pf, Lambda1f, K1f, Lambda2f, K2f, SampleCount,
		  CanonBounds } );


% Weibull-ZI with no sample count:
canonicalise_pdf_based_spec( _LS={ weibull_zi, Lambda, K, P } ) ->
	canonicalise_pdf_based_spec( { weibull_zi, Lambda, K, P,
								   ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec( LS={ weibull_zi, Lambda, K, P, SampleCount } ) ->

	% Needed to compute support:
	{ WbPDFFun, Lambdaf, Kf, Pf } = get_weibull_zi_pdf( Lambda, K, P, LS ),

	SampleBounds = math_utils:compute_support( WbPDFFun ),

	% To be shared with next clause, fun zieated once:
	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_zi, Lambdaf, Kf, Pf, SampleCount, SampleBounds } );

% Full information available for ZI:
canonicalise_pdf_based_spec(
		LS={ weibull_zi, Lambda, K, P, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	{ WbPDFFun, Lambdaf, Kf, Pf } = get_weibull_zi_pdf( Lambda, K, P, LS ),

	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_zi, Lambdaf, Kf, Pf, SampleCount, CanonBounds } );


% Then the beta section:
%
% Beta-2p with no sample count:
canonicalise_pdf_based_spec( _LS={ beta_2p, Alpha, Beta } ) ->
	canonicalise_pdf_based_spec(
		{ beta_2p, Alpha, Beta, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec( LS={ beta_2p, Alpha, Beta, SampleCount } ) ->
	% Needed to compute support (no a priori bounds):
	{ BetaPDFFun, Alphaf, Betaf } = get_beta_2p_pdf( Alpha, Beta, LS ),

	% As defined only on the [0,1] interval:
	SampleBounds = math_utils:compute_support( BetaPDFFun, _MinBound=0.0,
											   _MaxBound=1.0 ),

	% To be shared with next clause:
	canonicalise_beta_spec_with( BetaPDFFun,
		{ beta_2p, Alphaf, Betaf, SampleCount, SampleBounds } );

% Full information available for 2p:
canonicalise_pdf_based_spec(
		LS={ beta_2p, Alpha, Beta, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	% Checks Alphaf and Betaf:
	{ BetaPDFFun, Alphaf, Betaf } = get_beta_2p_pdf( Alpha, Beta, LS ),

	canonicalise_beta_spec_with( BetaPDFFun,
		{ beta_2p, Alphaf, Betaf, SampleCount, CanonBounds } );



% Arbitrary laws:
%
% No sample count defined here, using default one:
canonicalise_pdf_based_spec( _LS={ arbitrary, AnyName, _PDFInfo=LawFun } )
									when is_function( LawFun ) ->
	CountPDFInfo = { LawFun, ?default_pdf_sample_count },
	canonicalise_pdf_based_spec( { arbitrary, AnyName, CountPDFInfo } );

% Here, not having bounds (support) specified; determining them:
canonicalise_pdf_based_spec( _LS={ arbitrary, AnyName,
								  _PDFInfo={ LawFun, SampleCount } } ) ->

	SampleBounds = math_utils:compute_support( LawFun ),

	canonicalise_pdf_based_spec( { arbitrary, AnyName,
								  { LawFun, SampleCount, SampleBounds } } );

% Main clause, full information available:
canonicalise_pdf_based_spec( _LS={ arbitrary, AnyName,
		_PDFInfo={ LawFun, SampleCount, SampleBounds } } )
			                        when is_function( LawFun ) ->

	check_sample_count( SampleCount ),

	CanonBounds = { CMinBound, CMaxBound } =
		math_utils:canonicalise_bounds( SampleBounds ),

	CanonPDFInfo = { LawFun, SampleCount, CanonBounds },

	CanonSpec = { arbitrary, text_utils:ensure_binary( AnyName ),
				  CanonPDFInfo },

	Increment = ( CMaxBound - CMinBound ) / SampleCount,

	% Increment returned for direct reuse:
	{ CanonSpec, Increment };

canonicalise_pdf_based_spec( Other ) ->
	throw( { unsupported_random_law_spec, Other } ).



% @doc Checks that the specified term is a suitable lambda for an exponential
% law and returns it.
%
-spec check_exponential_lambda( term() ) -> rate().
check_exponential_lambda( Lambda ) when Lambda > 0 ->
	float( Lambda );

check_exponential_lambda( Other ) ->
	throw( { invalid_exponential_lambda, Other } ).



% @doc Checks that the specified term is a suitable gamma for an exponential-2p
% law and returns it.
%
-spec check_exponential_gamma( term() ) -> rate().
check_exponential_gamma( Gamma ) when Gamma > 0 ->
	float( Gamma );

check_exponential_gamma( Other ) ->
	throw( { invalid_exponential_2p_gamma, Other } ).



% @doc Checks that the specified term is a suitable mu (hence mean) for a
% gaussian law and returns it.
%
-spec check_gaussian_mu( term() ) -> mean().
check_gaussian_mu( Mu ) when is_number( Mu ) ->
	float( Mu );

check_gaussian_mu( Other ) ->
	throw( { invalid_mu_gaussian, Other } ).



% @doc Checks that the specified term is a suitable sigma (hence standard
% deviation) for a gaussian law and returns it.
%
-spec check_gaussian_sigma( term() ) -> standard_deviation().
check_gaussian_sigma( Sigma ) when Sigma >= 0 ->
	float( Sigma );

check_gaussian_sigma( Other ) ->
	throw( { invalid_sigma_gaussian, Other } ).



% @doc Returns the corresponding Exponential-1p PDF, after having checked
% user-supplied parameters.
%
-spec get_exponential_1p_pdf( term(), term() ) ->
			{ exponential_1p_pdf(), rate() }.
get_exponential_1p_pdf( Lambda, _LS ) ->

	Lambdaf = check_exponential_lambda( Lambda ),

	ExpPDFFun = fun( S ) -> exponential_1p_pdf( S, Lambdaf ) end,

	{ ExpPDFFun, Lambdaf }.



% @doc Returns the corresponding Exponential-2p PDF, after having checked
% user-supplied parameters.
%
-spec get_exponential_2p_pdf( term(), term(), term() ) ->
			{ exponential_2p_pdf(), rate(), rate() }.
get_exponential_2p_pdf( Lambda, Gamma, _LS ) ->

	Lambdaf = check_exponential_lambda( Lambda ),
	Gammaf = check_exponential_gamma( Gamma ),

	ExpPDFFun = fun( S ) -> exponential_2p_pdf( S, Lambdaf, Gammaf ) end,

	{ ExpPDFFun, Lambdaf, Gammaf }.



% @doc Returns the corresponding Gamma-2p PDF, after having checked
% user-supplied parameters.
%
-spec get_gamma_2p_pdf( term(), term(), term() ) ->
		{ gamma_2p_pdf(), positive_float(), positive_float() }.
get_gamma_2p_pdf( K, Theta, LS ) ->

	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	Theta > 0.0 orelse throw( { invalid_theta, Theta, LS } ),

	Kf = float( K ),
	Thetaf = float( Theta ),

	GamPDFFun = fun( S ) -> gamma_2p_pdf( S, Kf, Thetaf ) end,

	{ GamPDFFun, Kf, Thetaf }.



% @doc Returns the corresponding Gamma-3p PDF, after having checked
% user-supplied parameters.
%
-spec get_gamma_3p_pdf( term(), term(), term(), term() ) ->
		{ gamma_3p_pdf(), positive_float(), positive_float(), float() }.
get_gamma_3p_pdf( Alpha, Beta, Theta, LS ) ->

	Alpha > 0.0 orelse throw( { invalid_alpha, Alpha, LS } ),
	Beta > 0.0 orelse throw( { invalid_beta, Beta, LS } ),

	Alphaf = float( Alpha ),
	Betaf = float( Beta ),
	Thetaf = float( Theta ),

	GamPDFFun = fun( S ) -> gamma_3p_pdf( S, Alphaf, Betaf, Thetaf ) end,

	{ GamPDFFun, Alphaf, Betaf, Thetaf }.



% @doc Returns the corresponding Gumbel-2p PDF, after having checked
% user-supplied parameters.
%
-spec get_gumbel_2p_pdf( term(), term(), term() ) ->
		{ gumbel_2p_pdf(), float(), positive_float() }.
get_gumbel_2p_pdf( Mu, Beta, LS ) ->

	Beta > 0.0 orelse throw( { invalid_beta, Beta, LS } ),

	Muf = float( Mu ),
	Betaf = float( Beta ),

	GumPDFFun = fun( S ) -> gumbel_2p_pdf( S, Muf, Betaf ) end,

	{ GumPDFFun, Muf, Betaf }.



% @doc Returns the corresponding Log-logistic-2p PDF, after having checked
% user-supplied parameters.
%
-spec get_loglogistic_2p_pdf( term(), term(), term() ) ->
		{ loglogistic_2p_pdf(), positive_float(), positive_float() }.
get_loglogistic_2p_pdf( K, Theta, LS ) ->

	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	Theta > 0.0 orelse throw( { invalid_theta, Theta, LS } ),

	Kf = float( K ),
	Thetaf = float( Theta ),

	LogPDFFun = fun( S ) -> loglogistic_2p_pdf( S, Kf, Thetaf ) end,

	{ LogPDFFun, Kf, Thetaf }.



% @doc Returns the corresponding Log-logistic-3p PDF, after having checked
% user-supplied parameters.
%
-spec get_loglogistic_3p_pdf( term(), term(), term(), term() ) ->
		{ loglogistic_3p_pdf(), positive_float(), positive_float(),
		  positive_float() }.
get_loglogistic_3p_pdf( Alpha, Beta, Theta, LS ) ->

	Alpha > 0.0 orelse throw( { invalid_alpha, Alpha, LS } ),
	Beta  > 0.0 orelse throw( { invalid_beta,  Beta, LS } ),
	Theta > 0.0 orelse throw( { invalid_theta, Theta, LS } ),

	Alphaf = float( Alpha ),
	Betaf = float( Beta ),
	Thetaf = float( Theta ),

	LogPDFFun = fun( S ) -> loglogistic_3p_pdf( S, Alphaf, Betaf, Thetaf ) end,

	{ LogPDFFun, Alphaf, Betaf, Thetaf }.



% @doc Returns the corresponding Log-normal-2p PDF, after having checked
% user-supplied parameters.
%
-spec get_lognormal_2p_pdf( term(), term(), term() ) ->
		{ lognormal_2p_pdf(), float(), positive_float() }.
get_lognormal_2p_pdf( Mu, Sigma, LS ) ->

	Sigma > 0.0 orelse throw( { invalid_sigma, Sigma, LS } ),

	Muf = float( Mu ),
	Sigmaf = float( Sigma ),

	LogPDFFun = fun( S ) -> lognormal_2p_pdf( S, Muf, Sigmaf ) end,

	{ LogPDFFun, Muf, Sigmaf }.



% @doc Returns the corresponding Log-normal-3p PDF, after having checked
% user-supplied parameters.
%
-spec get_lognormal_3p_pdf( term(), term(), term(), term() ) ->
		{ lognormal_3p_pdf(), float(), positive_float(), float() }.
get_lognormal_3p_pdf( Mu, Sigma, Theta, LS ) ->

	Sigma  > 0.0 orelse throw( { invalid_sigma,  Sigma, LS } ),

	Muf = float( Mu ),
	Sigmaf = float( Sigma ),
	Thetaf = float( Theta ),

	LogPDFFun = fun( S ) -> lognormal_3p_pdf( S, Muf, Sigmaf, Thetaf ) end,

	{ LogPDFFun, Muf, Sigmaf, Thetaf }.



% @doc Returns the corresponding Weibull-2p PDF, after having checked
% user-supplied parameters.
%
-spec get_weibull_2p_pdf( term(), term(), term() ) ->
		{ weibull_2p_pdf(), positive_float(), positive_float() }.
get_weibull_2p_pdf( K, Lambda, LS ) ->

	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	Lambda > 0.0 orelse throw( { invalid_lambda, Lambda, LS } ),

	Kf = float( K ),
	Lambdaf = float( Lambda ),

	WbPDFFun = fun( S ) -> weibull_2p_pdf( S, Kf, Lambdaf ) end,

	{ WbPDFFun, Kf, Lambdaf }.



% @doc Returns the corresponding Weibull-3p PDF, after having checked
% user-supplied parameters.
%
-spec get_weibull_3p_pdf( term(), term(), term(), term() ) ->
		{ weibull_3p_pdf(), positive_float(), positive_float(), float() }.
get_weibull_3p_pdf( K, Lambda, Gamma, LS ) ->

	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	Lambda > 0.0 orelse throw( { invalid_lambda, Lambda, LS } ),

	Kf = float( K ),
	Lambdaf = float( Lambda ),
	Gammaf = float( Gamma ),

	WbPDFFun = fun( S ) -> weibull_3p_pdf( S, Kf, Lambdaf, Gammaf ) end,

	{ WbPDFFun, Kf, Lambdaf, Gammaf }.



% @doc Returns the corresponding Weibull-CR PDF, after having checked
% user-supplied parameters.
%
-spec get_weibull_cr_pdf( term(), term(), term(), term() ) ->
		{ weibull_cr_pdf(), positive_float(), positive_float(),
		  positive_float() }.
get_weibull_cr_pdf( Lambda, K, Theta, LS ) ->

	Lambda > 0.0 orelse throw( { invalid_lambda, Lambda, LS } ),
	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	Theta > 0.0 orelse throw( { invalid_theta, Theta, LS } ),

	Lambdaf = float( Lambda ),
	Kf = float( K ),
	Thetaf = float( Theta ),

	WbPDFFun = fun( S ) -> weibull_cr_pdf( S, Lambdaf, Kf, Thetaf ) end,

	{ WbPDFFun, Lambdaf, Kf, Thetaf }.



% @doc Returns the corresponding Weibull-DS PDF, after having checked
% user-supplied parameters.
%
-spec get_weibull_ds_pdf( term(), term(), term(), term() ) ->
		{ weibull_ds_pdf(), positive_float(), positive_float(),
		  positive_float() }.
get_weibull_ds_pdf( Lambda, K, Sigma, LS ) ->

	Lambda > 0.0 orelse throw( { invalid_lambda, Lambda, LS } ),
	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	Sigma > 0.0 orelse throw( { invalid_sigma, Sigma, LS } ),

	Lambdaf = float( Lambda ),
	Kf = float( K ),
	Sigmaf = float( Sigma ),

	WbPDFFun = fun( S ) -> weibull_ds_pdf( S, Lambdaf, Kf, Sigmaf ) end,

	{ WbPDFFun, Lambdaf, Kf, Sigmaf }.




% @doc Returns the corresponding Weibull-DSZI PDF, after having checked
% user-supplied parameters.
%
-spec get_weibull_dszi_pdf( term(), term(), term(), term(), term() ) ->
		{ weibull_dszi_pdf(), positive_float(), positive_float(),
		  positive_float(), positive_float() }.
get_weibull_dszi_pdf( Lambda, K, Sigma, Theta, LS ) ->

	Lambda > 0.0 orelse throw( { invalid_lambda, Lambda, LS } ),
	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	Sigma > 0.0 orelse throw( { invalid_sigma, Sigma, LS } ),
	Theta > 0.0 orelse throw( { invalid_theta, Theta, LS } ),

	Lambdaf = float( Lambda ),
	Kf = float( K ),
	Sigmaf = float( Sigma ),
	Thetaf = float( Theta ),

	WbPDFFun = fun( S ) ->
				weibull_dszi_pdf( S, Lambdaf, Kf, Sigmaf, Thetaf )
			   end,

	{ WbPDFFun, Lambdaf, Kf, Sigmaf, Thetaf }.



% @doc Returns the corresponding Weibull-Mixture PDF, after having checked
% user-supplied parameters.
%
-spec get_weibull_mixture_pdf( term(), term(), term(), term(), term(),
							   term() ) ->
		{ weibull_mixture_pdf(), positive_float(), positive_float(),
		  positive_float(), positive_float() }.
get_weibull_mixture_pdf( P, Lambda1, K1, Lambda2, K2, LS ) ->

	P > 0.0 orelse throw( { invalid_p, P, LS } ),

	Lambda1 > 0.0 orelse throw( { invalid_lambda1, Lambda1, LS } ),
	K1 > 0.0 orelse throw( { invalid_k1, K1, LS } ),

	Lambda2 > 0.0 orelse throw( { invalid_lambda2, Lambda2, LS } ),
	K2 > 0.0 orelse throw( { invalid_k2, K2, LS } ),

	Pf = float( P ),

	Lambda1f = float( Lambda1 ),
	K1f = float( K1 ),

	Lambda2f = float( Lambda2 ),
	K2f = float( K2 ),

	WbPDFFun = fun( S ) ->
				weibull_mixture_pdf( S, Pf, Lambda1f, K1f, Lambda2f, K2f )
			   end,

	{ WbPDFFun, Pf, Lambda1f, K1f, Lambda2f, K2f }.



% @doc Returns the corresponding Weibull-ZI PDF, after having checked
% user-supplied parameters.
%
-spec get_weibull_zi_pdf( term(), term(), term(), term() ) ->
		{ weibull_zi_pdf(), positive_float(), positive_float(),
		  positive_float(), positive_float() }.
get_weibull_zi_pdf( Lambda, K, P, LS ) ->

	Lambda > 0.0 orelse throw( { invalid_lambda, Lambda, LS } ),
	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	P > 0.0 orelse throw( { invalid_p, P, LS } ),

	Lambdaf = float( Lambda ),
	Kf = float( K ),
	Pf = float( P ),

	WbPDFFun = fun( S ) ->
				weibull_zi_pdf( S, Lambdaf, Kf, Pf )
			   end,

	{ WbPDFFun, Lambdaf, Kf, Pf }.



% @doc Returns the corresponding Beta-2p PDF, after having checked user-supplied
% parameters.
%
-spec get_beta_2p_pdf( term(), term(), term() ) ->
		{ beta_2p_pdf(), positive_float(), positive_float() }.
get_beta_2p_pdf( Alpha, Beta, LS ) ->

	Alpha > 0.0 orelse throw( { invalid_alpha, Alpha, LS } ),
	Beta > 0.0 orelse throw( { invalid_beta, Beta, LS } ),

	Alphaf = float( Alpha ),
	Betaf = float( Beta ),

	BetaPDFFun = fun( S ) -> beta_2p_pdf( S, Alphaf, Betaf ) end,

	{ BetaPDFFun, Alphaf, Betaf }.



% Common for all Exponential functions.
%
% Bounds supposed to be already canonic.
%
-spec canonicalise_exponential_spec_with( exponential_2p_pdf(), tuple() ) ->
			{ full_exponential_law_spec(), increment(), exponential_2p_pdf() }.
canonicalise_exponential_spec_with( ExpPDFFun,
		{ exponential_2p, Lambdaf, Gammaf, SampleCount,
		  Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising an Exponential-2p law of "
			"lambda=~f and gamma=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Lambdaf, Gammaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { exponential_2p, Lambdaf, Gammaf, SampleCount, Bounds },

	{ CanonSpec, Inc, ExpPDFFun }.



% Common for all Gamma functions.
%
% Bounds supposed to be already canonic.
%
-spec canonicalise_gamma_spec_with( gamma_pdf(), tuple() ) ->
			{ full_gamma_law_spec(), increment(), gamma_pdf() }.
canonicalise_gamma_spec_with( GamPDFFun,
		{ gamma_2p, Kf, Thetaf, SampleCount, Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Gamma-2p law of k=~f "
			"and theta=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Kf, Thetaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { gamma_2p, Kf, Thetaf, SampleCount, Bounds },

	{ CanonSpec, Inc, GamPDFFun };

canonicalise_gamma_spec_with( GamPDFFun,
		{ gamma_3p, Alphaf, Betaf, Thetaf, SampleCount,
		  Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Gamma-3p law of alpha=~f, "
			"beta=~f and theta=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Alphaf, Betaf, Thetaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { gamma_3p, Alphaf, Betaf, Thetaf, SampleCount, Bounds },

	{ CanonSpec, Inc, GamPDFFun }.



% Common for all Gumbel functions.
%
% Bounds supposed to be already canonic.
%
-spec canonicalise_gumbel_spec_with( gumbel_pdf(), tuple() ) ->
			{ full_gumbel_2p_law_spec(), increment(), gumbel_pdf() }.
canonicalise_gumbel_spec_with( GumbelPDFFun,
		{ gumbel_2p, Muf, Betaf, SampleCount, Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Gumbel-2p law of "
			"mu=~f and beta=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Muf, Betaf, math_utils:bounds_to_string( Bounds ), SampleCount,
			  Inc ] ) ),

	CanonSpec = { gumbel_2p, Muf, Betaf, SampleCount, Bounds },

	{ CanonSpec, Inc, GumbelPDFFun }.



% Common for all Log-logistic functions.
%
% Bounds supposed to be already canonic.
%
-spec canonicalise_loglogistic_spec_with( loglogistic_pdf(), tuple() ) ->
			{ full_loglogistic_law_spec(), increment(), loglogistic_pdf() }.
canonicalise_loglogistic_spec_with( LogPDFFun,
		{ loglogistic_2p, Alphaf, Betaf, SampleCount, Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Log-logistic-2p law "
			"of alpha=~f and beta=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Alphaf, Betaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { loglogistic_2p, Alphaf, Betaf, SampleCount, Bounds },

	{ CanonSpec, Inc, LogPDFFun };

canonicalise_loglogistic_spec_with( LogPDFFun,
		{ loglogistic_3p, Alphaf, Betaf, Thetaf, SampleCount,
		  Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Log-logistic-3p law "
			"of alpha=~f, beta=~f and theta=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Alphaf, Betaf, Thetaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { loglogistic_3p, Alphaf, Betaf, Thetaf, SampleCount, Bounds },

	{ CanonSpec, Inc, LogPDFFun }.



% Common for all Log-normal functions.
%
% Bounds supposed to be already canonic.
%
-spec canonicalise_lognormal_spec_with( lognormal_pdf(), tuple() ) ->
			{ full_lognormal_law_spec(), increment(), lognormal_pdf() }.
canonicalise_lognormal_spec_with( LogPDFFun,
		{ lognormal_2p, Muf, Sigmaf, SampleCount, Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Log-normal-2p law "
			"of mu=~f and sigma=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Muf, Sigmaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { lognormal_2p, Muf, Sigmaf, SampleCount, Bounds },

	{ CanonSpec, Inc, LogPDFFun };

canonicalise_lognormal_spec_with( LogPDFFun,
		{ lognormal_3p, Muf, Sigmaf, Thetaf, SampleCount,
		  Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Log-normal-3p law "
			"of mu=~f, sigma=~f and theta=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Muf, Sigmaf, Thetaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { lognormal_3p, Muf, Sigmaf, Thetaf, SampleCount, Bounds },

	{ CanonSpec, Inc, LogPDFFun }.




% Common for all Weibull functions.
%
% Bounds supposed to be already canonic.
%
-spec canonicalise_weibull_spec_with( weibull_pdf(), tuple() ) ->
			{ full_weibull_law_spec(), increment(), weibull_pdf() }.
canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_2p, Kf, Lambdaf, SampleCount, Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Weibull-2p law of k=~f "
			"and lambda=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Kf, Lambdaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { weibull_2p, Kf, Lambdaf, SampleCount, Bounds },

	{ CanonSpec, Inc, WbPDFFun };


canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_3p, Kf, Lambdaf, Gammaf, SampleCount,
		  Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Weibull-3p law of k=~f, "
			"lambda=~f and gamma=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Kf, Lambdaf, Gammaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { weibull_3p, Kf, Lambdaf, Gammaf, SampleCount, Bounds },

	{ CanonSpec, Inc, WbPDFFun };


canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_cr, Lambdaf, Kf, Thetaf, SampleCount,
		  Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Weibull-CR law of lambda=~f, "
			"k=~f and theta=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Lambdaf, Kf, Thetaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { weibull_cr, Lambdaf, Kf, Thetaf, SampleCount, Bounds },

	{ CanonSpec, Inc, WbPDFFun };


canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_ds, Lambdaf, Kf, Sigmaf, SampleCount,
		  Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Weibull-DS law of lambda=~f, "
			"k=~f and sigma=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Lambdaf, Kf, Sigmaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { weibull_ds, Lambdaf, Kf, Sigmaf, SampleCount, Bounds },

	{ CanonSpec, Inc, WbPDFFun };


canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_dszi, Lambdaf, Kf, Sigmaf, Thetaf, SampleCount,
		  Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Weibull-DSZI law "
			"of lambda=~f, k=~f, sigma=~f and theta=~f, "
			"discretised on interval ~ts with ~B points (increment: ~f).",
			[ Lambdaf, Kf, Sigmaf, Thetaf,
			  math_utils:bounds_to_string( Bounds ), SampleCount, Inc ] ) ),

	CanonSpec = { weibull_dszi, Lambdaf, Kf, Sigmaf, Thetaf, SampleCount,
				  Bounds },

	{ CanonSpec, Inc, WbPDFFun };


canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_mixture, Pf, Lambda1f, K1f, Lambda2f, K2f, SampleCount,
		  Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Weibull-Mixture law "
			"of p=~f, lambda1=~f, k1=~f, lambda2=~f and k2=~f,  "
			"discretised on interval ~ts with ~B points (increment: ~f).",
			[ Pf, Lambda1f, K1f, Lambda2f, K2f,
			  math_utils:bounds_to_string( Bounds ), SampleCount, Inc ] ) ),

	CanonSpec = { weibull_mixture, Pf, Lambda1f, K1f, Lambda2f, K2f,
				  SampleCount, Bounds },

	{ CanonSpec, Inc, WbPDFFun };


canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_zi, Lambdaf, Kf, Pf, SampleCount, Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Weibull-ZI law "
			"of lambda=~f, k=~f and p=~f, "
			"discretised on interval ~ts with ~B points (increment: ~f).",
			[ Lambdaf, Kf, Pf,
			  math_utils:bounds_to_string( Bounds ), SampleCount, Inc ] ) ),

	CanonSpec = { weibull_zi, Lambdaf, Kf, Pf, SampleCount, Bounds },

	{ CanonSpec, Inc, WbPDFFun }.




% Common for all Beta functions.
%
% Bounds supposed to be already canonic.
%
-spec canonicalise_beta_spec_with( beta_pdf(), tuple() ) ->
			{ full_beta_law_spec(), increment(), beta_pdf() }.
canonicalise_beta_spec_with( BetaPDFFun,
		{ beta_2p, Alphaf, Betaf, SampleCount, Bounds={ Min, Max } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( Max - Min ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Beta-2p law of "
			"alpha=~f and beta=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Alphaf, Betaf, math_utils:bounds_to_string( Bounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { beta_2p, Alphaf, Betaf, SampleCount, Bounds },

	{ CanonSpec, Inc, BetaPDFFun }.



% @doc Returns a new sample drawn from the discrete probability distribution
% specified through its (constant) law data (which is thus not returned), this
% table having been obtained initially (and once for all) from its random
% specification (see initialise_law/1). Only the state of the internal (uniform)
% random generator is (transparently) modified.
%
% Each sample is generated in constant time O(1) time with regard to the number
% of samples declared in the corresponding distribution.
%
% Such a generation depends (and modifies) the state of the underlying uniform
% random generator (e.g. see start_random_source/0); precisely each non-uniform
% sampling results in two underlying uniform samples to be drawn.
%
-spec get_sample_from( random_law_data() ) -> sample().
% All distributions covered by an alias table can just be handled by the default
% case, so we concentrate on those which cannot be "directly" sampled:
get_sample_from( { _LawData={ uniform, Min, Max }, undefined } ) ->
	get_uniform_floating_point_value( Min, Max );

get_sample_from( { _LawData={ integer_uniform, Nmin, Nmax }, undefined } ) ->
	get_uniform_value( Nmin, Nmax );


get_sample_from( { _LawData={ exponential_1p, Lambda }, undefined } ) ->
	get_exponential_1p_value( Lambda );

get_sample_from( { _LawData={ exponential, Lambda }, undefined } ) ->
	get_exponential_1p_value( Lambda );

get_sample_from( { _LawData={ positive_integer_exponential_1p, Lambda },
				   undefined } ) ->
	get_positive_integer_exponential_1p_value( Lambda );


get_sample_from( { _LawData={ normal_2p, Mu, Sigma }, undefined } ) ->
	get_gaussian_value( Mu, Sigma );

get_sample_from( { _LawData={ gaussian, Mu, Sigma }, undefined } ) ->
	get_gaussian_value( Mu, Sigma );

get_sample_from( { _LawData={ positive_integer_gaussian, Mu, Sigma },
				   undefined } ) ->
	get_positive_integer_gaussian_value( Mu, Sigma );

% Such a default handles all distributions based on an alias table:
get_sample_from( { _AnyRandomLawData, MaybeAliasTable } )
									when MaybeAliasTable =/= undefined ->
	get_sample_from_table( MaybeAliasTable );

get_sample_from( { OtherRandomLawData, _MaybeAliasTable=undefined } ) ->
	throw( { unsupported_direct_distribution, OtherRandomLawData } );

get_sample_from( Other ) ->
	throw( { unsupported_random_law_data, Other } ).




% @doc Returns the specified number of samples drawn according to the specified
% law data.
%
% Refer to get_sample_from/1 for more details.
%
-spec get_samples_from( sample_count(), random_law_data() ) -> [ sample() ].
get_samples_from( Count, LawData ) ->

	trace_utils:debug_fmt( "Drawing ~B samples from ~ts.",
						   [ Count, law_data_to_string( LawData ) ] ),

	% Laws are static:
	[ get_sample_from( LawData ) || _ <- lists:seq( 1, Count ) ].



% @doc Returns a new sample drawn from the discrete probability distribution
% specified through its (constant) alias table (which is thus not returned),
% this table having been obtained initially (and once for all) from
% generate_alias_table_from/1.
%
% Each sample is generated in constant time O(1) time with regard to the number
% of samples declared in the corresponding distribution.
%
% Such a generation depends (and modifies) the state of the underlying uniform
% random generator (e.g. see start_random_source/0); precisely each non-uniform
% sampling results in two underlying uniform samples to be drawn.
%
-spec get_sample_from_table( alias_table() ) -> sample().
get_sample_from_table( #alias_table{ entry_count=EntryCount,
									 sample_values=SampleValueArray,
									 indexes=IndexArray,
									 prob_likes=ProbLikeArray } ) ->

	% Thus uniform in [0, EntryCount-1]:
	PLIdx = get_uniform_value( EntryCount ) - 1,

	% Thus uniform in [0.0, 1.0[:
	P = get_uniform_value(),

	SampleIdx = case P =< array:get( PLIdx, ProbLikeArray ) of

		true ->
			PLIdx;

		_ ->
			array:get( PLIdx, IndexArray )

	end,

	array:get( SampleIdx, SampleValueArray ).



% Extra PDFs, which are notably useful for reliability-related computations.
%
% See also:
% https://reliability.readthedocs.io/en/latest/Equations%20of%20supported%20distributions.html
%
% Each PDF could be implemented according to either of these two approaches:
% A: it is simply defined analytically, and then sampled
% B: it is defined directly in terms of a another, more fundamental PDF, with no
% sampling



% Exponential-1p distribution:
%
% See https://en.wikipedia.org/wiki/Exponential_distribution.
%
% Lambda > 0 is the parameter of the distribution, often called the rate
% parameter.
%
-spec exponential_1p_pdf( positive_float_sample(), rate() ) -> probability().
exponential_1p_pdf( S, Lambda ) when S >= 0.0 ->
	% f(x; λ) = λ * e^(-λ * x)
	Lambda * exp( - S * Lambda );

exponential_1p_pdf( _S, _Lambda ) -> % when S < 0.0 ->
	0.0.



% Exponential-2p distribution:
%
% See https://en.wikipedia.org/wiki/Exponential_distribution.
%
% Lambda > 0 and Gamma > 0 are the parameters of the distribution, often called
% the rate parameters.
%
-spec exponential_2p_pdf( positive_float_sample(), rate(), rate() ) ->
			probability().
exponential_2p_pdf( S, Lambda, Gamma ) when S >= 0.0 ->
	% f(x; λ, γ) = λ * e^(-γ*x)
	Lambda * exp( - S * Gamma );

exponential_2p_pdf( _S, _Lambda, _Gamma ) -> % when S < 0.0 ->
	0.0.



% Gamma-2p distribution:
%
% See https://en.wikipedia.org/wiki/Gamma_distribution.
%
% K > 0 is the shape parameter and Theta > 0 is the scale parameter.
%
-spec gamma_2p_pdf( positive_float_sample(), shape(), scale() ) ->
															probability().
gamma_2p_pdf( S, K, Theta ) when S >= 0.0 ->
	% f(x; k, θ) = (1 / (θ^k * Γ(k))) * x^(k-1) * e^(-x/θ)
	( 1 / ( pow( Theta, K ) * math_utils:gamma( K ) )
		* pow( S, K-1 ) * exp(-S/Theta) );

gamma_2p_pdf( _S, _Lambda, _Gamma ) -> % when S < 0.0 ->
	0.0.



% Gamma-3p distribution:
%
% See https://en.wikipedia.org/wiki/Gamma_distribution.
%
% Alpha > 0 and Beta > 0 are the shape parameters, and Theta > 0 is the scale
% parameter.
%
-spec gamma_3p_pdf( positive_float_sample(), shape(), shape(), scale() ) ->
															probability().
gamma_3p_pdf( S, Alpha, Beta, Theta ) when S >= 0.0 ->
	% f(x; α, β, θ) = (1 / (θ^β * Γ(α/β))) * x^(α-1) * e^(-x/θ)
	( 1 / ( pow( Theta, Beta ) * math_utils:gamma( Alpha / Beta ) )
		* pow( S, Alpha-1 ) * exp( -S / Theta ) );

gamma_3p_pdf( _S, _Alpha, _Beta, _Theta ) -> % when S < 0.0 ->
	0.0.



% Gaussian (normal) distribution:
%
% See https://en.wikipedia.org/wiki/Normal_distribution.
%
% Mu is the mean or expectation of the distribution (and also its median and
% mode), while Sigma is its standard deviation.
%
-spec gaussian_pdf( positive_float_sample(), mean(), standard_deviation() ) ->
								probability().
gaussian_pdf( S, Mu, Sigma ) ->
	% For a normalised version thereof:
	% f(x; μ, σ) = (1 / (σ*sqrt(2π))) * e^-((x - μ)^2 / (2*σ^2))
	1.0 / ( Sigma * sqrt( 2*pi() ) )
		* exp( - pow( ( S - Mu ) / Sigma, 2 ) / 2 ).



% Gumbel-2p distribution:
%
% See https://en.wikipedia.org/wiki/Gumbel_distribution.
%
% Its support is for a sample S in R.
%
% Determined by 2 parameters:
% - Mu, in R, the location parameter
% - Beta > 0, the scale parameter
%
-spec gumbel_2p_pdf( float_sample(), float(), positive_float() ) ->
												probability().
gumbel_2p_pdf( S, Mu, Beta ) ->
	% f(x; μ, β) = (1 / β) * e^((μ-x)/β) * e^-e^((μ-x)/β)
	Exp = exp( ( Mu - S ) / Beta ),

	Exp / Beta * exp( -Exp ).



% Log-logistic-2p distribution:
%
% See https://en.wikipedia.org/wiki/Log-logistic_distribution.
%
% Alpha > 0 is the scale parameter and Beta > 0 is the shape parameter.
%
-spec loglogistic_2p_pdf( positive_float_sample(), positive_float(),
						  positive_float() ) ->	probability().
loglogistic_2p_pdf( S, Alpha, Beta ) when S >= 0.0 ->
	% f(x; c, σ) = (c / σ) * (x / σ)^(c-1) * (1 + (x / σ)^c)^-2
	% with x -> S, c -> Alpha, σ -> Beta:
	%
	% f(S; Alpha, Beta) = (Alpha / Beta) * (S / Beta)^(Alpha-1) * (1 + (S /
	% Beta)^Alpha)^-2

	F = S / Beta,
	T = pow( F, Alpha-1 ),
	% No sqr/1:
	( Alpha / Beta ) * T * pow( 1 + F*T, -2 );


loglogistic_2p_pdf( _S, _Alpha, _Beta ) -> % when S < 0.0 ->
	0.0.



% Log-logistic-3p distribution:
%
% See https://en.wikipedia.org/wiki/Log-logistic_distribution.
%
% Alpha > 0 is the scale parameter, Beta > 0 is the shape parameter, Theta is
% the last one.
%
-spec loglogistic_3p_pdf( positive_float_sample(), positive_float(),
						  positive_float(), positive_float() ) -> probability().
loglogistic_3p_pdf( S, Alpha, Beta, Theta ) when S >= 0.0 ->
	% f(x; c, σ, θ) = (c / (σ * θ)) * (x / σ)^(c-1) * (1 + (x / σ)^c)^-2 * (1 /
	% (θ^c * Γ(c))) * x^(c-1) * e^(-x/θ)
	% with x -> S, c -> Alpha, σ -> Beta, θ -> Theta:
	%
	% f(S; Alpha, Beta, Theta) = (Alpha / (Beta * Theta)) * (S / Beta)^(Alpha-1)
	% * (1 + (S / Beta)^Alpha)^-2 * (1 / (Theta^Alpha * Γ(Alpha))) * S^(Alpha-1)
	% * e^(-S/Theta)

	F = S / Beta,
	DecAlpha = Alpha - 1,
	T = pow( F, DecAlpha ),

	( Alpha / ( Beta * Theta ) ) * T * pow( 1 + F*T, -2 )
		* (1 / ( pow( Theta, Alpha ) * math_utils:gamma( Alpha ) ) )
		* pow( S, DecAlpha ) * exp( -S / Theta );

loglogistic_3p_pdf( _S, _Alpha, _Beta, _Theta ) -> % when S < 0.0 ->
	0.0.




% Log-normal-2p distribution:
%
% See https://en.wikipedia.org/wiki/Log-normal_distribution.
%
% Refer to lognormal_2p_law_spec/0 for further details.
%
-spec lognormal_2p_pdf( positive_float_sample(), float(), positive_float() ) ->
																probability().
% lognormal_2p_pdf( S, Mu, Sigma ) = lognormal_3p_pdf( S, Mu, Sigma, _Theta=1 ).
lognormal_2p_pdf( S, Mu, Sigma ) when S >= 0.0 ->
	% f(x; μ, σ) = (1 / (x*σ*sqrt(2π))) * e^-((ln(x) - μ)^2 / (2*σ^2))
	% with x -> S, μ -> Mu, σ -> Sigma:
	%
	( 1 / ( S * Sigma * ?sqrt_2_pi ) )
		* exp( -( pow( ln( S ) - Mu, 2 ) / (2*Sigma*Sigma) ) );

lognormal_2p_pdf( _S, _Mu, _Sigma ) -> % when S < 0.0 ->
	0.0.



% Log-normal-3p distribution:
%
% See https://en.wikipedia.org/wiki/Log-normal_distribution.
%
% Mu > 0 is the scale parameter, Sigma > 0 is the shape parameter, Theta is
% the last one.
%
-spec lognormal_3p_pdf( positive_float_sample(), float(), positive_float(),
						float() ) -> probability().
lognormal_3p_pdf( S, Mu, Sigma, Theta ) when S >= 0.0 ->
	% f(x; μ, σ, θ) = (1 / (x*σ*θ*sqrt(2π))) * e^-((ln(x) - μ)^2 / (2*σ^2))
	% with x -> S, μ -> Mu, σ -> Sigma, θ -> Theta:
	%
	( 1 / ( S * Sigma * Theta * ?sqrt_2_pi ) )
		* exp( -( pow( ln( S ) - Mu, 2 ) / (2*Sigma*Sigma) ) );

lognormal_3p_pdf( _S, _Mu, _Sigma, _Theta ) -> % when S < 0.0 ->
	0.0.



% Weibull-2p distribution:
%
% See https://en.wikipedia.org/wiki/Weibull_distribution.
%
% Its support is for a sample S>=0.0.
%
% Determined by 2 parameters:
% - K > 0 is the shape parameter (sometimes named beta)
% - Lambda > 0 is the scale parameter (sometimes named alpha)
%
% Being quite flexible, its proper parametrisation can cover many laws,
% including the exponential-1p law (K=1) and the Rayleigh law (K=2 and
% Lambda=sqrt(2).Sigma).
%
-spec weibull_2p_pdf( positive_float_sample(), positive_float(),
					  positive_float() ) -> probability().
weibull_2p_pdf( S, K, Lambda ) when S >= 0.0 ->
	% f(x; λ, k) = (k / λ) * (x / λ)^(k-1) * e^-((x / λ)^k)
	% with x -> S, λ -> Lambda, k -> K:
	%
	A = S / Lambda,
	K / Lambda * pow( A, K-1 ) * exp( -pow( A, K ) );

weibull_2p_pdf( _S, _K, _Lambda ) -> % when S < 0.0 ->
	0.0.


% Weibull-3p distribution:
%
% See https://en.wikipedia.org/wiki/Weibull_distribution.
%
% Its support is for a sample S>=Gamma.
%
% Determined by 3 parameters:
% - K > 0 is the shape parameter (sometimes named beta)
% - Lambda > 0 is the scale parameter (sometimes named alpha)
% - Gamma (in R) is the location parameter (or failure free life)
%
% Being quite flexible, its proper parametrisation can cover many laws,
% including the Weibull-2p ones (with Gamma=0).
%
-spec weibull_3p_pdf( positive_float_sample(), positive_float(),
					  positive_float(), float() ) -> probability().
weibull_3p_pdf( S, K, Lambda, Gamma ) when S >= 0.0 ->
	% f(x; λ, k, θ) = (k / λ) * ((x - θ) / λ)^(k-1) * e^-(((x - θ) / λ)^k)
	% with x -> S, λ -> Lambda, k -> K, θ -> Gamma:
	%
	A = ( S - Gamma ) / Lambda,
	K/Lambda * pow( A, K-1 ) * exp( -pow( A, K ) );

weibull_3p_pdf( _S, _K, _Lambda, _Gamma ) -> % when S < 0.0 ->
	0.0.



% Weibull-CR distribution:
%
% See https://en.wikipedia.org/wiki/Weibull_distribution.
%
% Determined by 3 parameters:
% - K > 0 is the shape parameter (sometimes named beta)
% - Lambda > 0 is the scale parameter (sometimes named alpha)
% - Theta (in R) is the location parameter (or failure free life)
%
-spec weibull_cr_pdf( positive_float_sample(), positive_float(),
					  positive_float(), positive_float() ) -> probability().
weibull_cr_pdf( S, Lambda, K, Theta ) when S >= 0.0 ->
	% f(x; λ, k, θ) = (k / λ) * (x / θ)^(k-1) * e^-((x / θ)^k - 1)
	%
	% With x -> S, λ -> Lambda, k -> K, θ -> Theta:
	% f(S; Lambda, K, Theta) = (K / Lambda) * (S / Theta)^(K-1) * e^-((S /
	% Theta)^K - 1)
	%
	A = S / Theta,
	P = pow( A, K-1 ),
	( K / Lambda ) * P * exp( -( P*A - 1 ) );

weibull_cr_pdf( _S, _Lambda, _K, _Theta ) -> % when S < 0.0 ->
	0.0.


% Weibull-DS distribution:
%
% See https://en.wikipedia.org/wiki/Weibull_distribution.
%
% Determined by 3 parameters:
% - K > 0 is the shape parameter (sometimes named beta)
% - Lambda > 0 is the scale parameter (sometimes named alpha)
% - Sigma > 0
%
-spec weibull_ds_pdf( positive_float_sample(), positive_float(),
					  positive_float(), positive_float() ) -> probability().
weibull_ds_pdf( S, Lambda, K, Sigma ) when S >= 0.0 ->
	% f(x; λ, k, σ) = (k / λ) * (x / λ)^(k-1) * e^-(((x / λ)^k
	%   + (x / σ)^k)^(-1/k))
	%
	% With x -> S, λ -> Lambda, k -> K, θ -> Sigma:
	% f(S; Lambda, K, Sigma) = (K / Lambda) * (S / Lambda)^(K-1)
	%    * e^-(((S / Lambda)^K + (S / Sigma)^K)^(-1/K))

	A = S / Lambda,

	P = pow( A, K-1 ),

	( K / Lambda ) * P
		* exp( -( pow( A*P + pow( S / Sigma, K ), -1 / K ) ) );

weibull_ds_pdf( _S, _Lambda, _K, _Sigma ) -> % when S < 0.0 ->
	0.0.



% Weibull-DSZI distribution:
%
% See https://en.wikipedia.org/wiki/Weibull_distribution.
%
% Determined by 4 parameters:
% - K > 0 is the shape parameter (sometimes named beta)
% - Lambda > 0 is the scale parameter (sometimes named alpha)
% - Sigma > 0
% - Theta > 0
%
% See, in the Python-Reliability library, in the Distributions module, the
% DSZI_Model class, notably:
% 'pdf = pdf0 * (self.DS - self.ZI)  # the DSZI formula for the PDF'.
%
-spec weibull_dszi_pdf( positive_float_sample(), positive_float(),
		positive_float(), positive_float(), positive_float() ) -> probability().
weibull_dszi_pdf( S, Lambda, K, Sigma, Theta ) when S >= 0.0 ->
	% f(x; λ, k, σ, θ) =
	%   (k / λ) * ((x - θ) / λ)^(k-1) * e^-(((x - θ) / λ)^k + (x / σ)^k)
	%
	% With x -> S, λ -> Lambda, k -> K, σ -> Sigma, θ -> Theta:
	% f(S; Lambda, K, Sigma, Theta) = (K / Lambda)
	%    * ((S - Theta) / Lambda)^(K-1)
	%    * e^-(((S - Theta) / Lambda)^K + (S / Sigma)^K)

	A = ( S - Theta ) / Lambda,

	P = pow( A, K-1 ),

	( K / Lambda ) * P
		* exp( -( pow( A*P, K ) + pow( S / Sigma, K ) ) );

weibull_dszi_pdf( _S, _Lambda, _K, _Sigma, _Theta ) -> % when S < 0.0 ->
	0.0.



% Weibull-Mixture distribution:
%
% See https://en.wikipedia.org/wiki/Weibull_distribution.
%
% Determined by 5 parameters:
% - P > 0, a kind of proportion between the two Weibull-2p functions (thus
% typically in [0,1])
% - K1 > 0 is the shape parameter (sometimes named beta) of the first Weibull
% - Lambda1 > 0 is the scale parameter (sometimes named alpha) of the first
% Weibull
% - K2 > 0 is the shape parameter (sometimes named beta) of the second Weibull
% - Lambd2a > 0 is the scale parameter (sometimes named alpha) of the second
% Weibull
%
-spec weibull_mixture_pdf( positive_float_sample(), positive_float(),
		positive_float(), positive_float(), positive_float(),
		positive_float() ) -> probability().
weibull_mixture_pdf( S, P, Lambda1, K1, Lambda2, K2 ) when S >= 0.0 ->
	% f(x; p, λ1, k1, λ2, k2) = p * (k1 / λ1) * (x / λ1)^(k1-1)
	%    * e^-((x / λ1)^k1) + (1-p) * (k2 / λ2) * (x / λ2)^(k2-1)
	%    * e^-((x / λ2)^k2)

	% With x -> S, p -> P, λ1 -> Lambda1, k1 -> K1, λ2 -> Lambda2, k2 -> K2
	% f(S; P, Lambda1, K1, Lambda2, K2) =
	%        P * (K1 / Lambda1) * (S / Lambda1)^(K1-1) * e^-((S / Lambda1)^K1)
	%  + (1-P) * (K2 / Lambda2) * (S / Lambda2)^(K2-1) * e^-((S / Lambda2)^K2)

	A1 = S / Lambda1,
	P1 = pow( A1, K1-1 ),

	A2 = S / Lambda2,
	P2 = pow( A2, K2-1 ),

	Res =     P * ( K1 / Lambda1 ) * P1 * exp( -A1*P1 )
		+ (1-P) * ( K2 / Lambda2 ) * P2 * exp( -A2*P2 ),

	Res;

weibull_mixture_pdf( _S, _P, _Lambda1, _K1, _Lambda2, _K2 ) -> % when S < 0.0 ->
	0.0.




% Weibull-ZI distribution:
%
% See https://en.wikipedia.org/wiki/Weibull_distribution.
%
% Determined by 3 parameters:
% - Lambda > 0 is the scale parameter (sometimes named alpha)
% - K > 0
% - P > 0, a kind of proportion between the Weibull-2p function and a Dirac
% distribution (thus typically in [0,1])
%
-spec weibull_zi_pdf( positive_float_sample(), positive_float(),
					  positive_float(), positive_float() ) -> probability().
weibull_zi_pdf( S, Lambda, K, P ) when S > 0.0 ->
	% f(x; λ, k, p) = (1-p) * (k / λ) * (x / λ)^(k-1) * e^-((x / λ)^k)
	%   + p * δ(x)

	% With x -> S, λ -> Lambda, k -> K, p -> P:
	% f(S; Lambda, K, P) = (1-P) * (K / Lambda) * (S / Lambda)^(K-1)
	%    * e^-((S / Lambda)^K) + P * δ(S)

	A = S / Lambda,

	Pow = pow( A, K-1 ),

	% Here S>0 hence δ(S)=0:
	(1-P) * ( K / Lambda ) * Pow * exp( -( A*Pow ) );

weibull_zi_pdf( S, Lambda, K, P ) when S == 0.0 ->
	% Here S=0 hence δ(0) is infinite and of integral 1.

	A = S / Lambda,

	Pow = pow( A, K-1 ),

	% We currently consider that δ(0)=1, and therefore:
	(1-P) * ( K / Lambda ) * Pow * exp( -( A*Pow ) ) + P;

weibull_zi_pdf( _S, _Lambda, _K, _P ) -> % when S < 0.0 ->
	0.0.



% Beta-2p distribution:
%
% See https://en.wikipedia.org/wiki/Beta_distribution.
%
% Its support is for a sample S in [0,1].
%
% Determined by 2 parameters:
% - Alpha > 0, a shape parameter
% - Beta > 0, another shape parameter
%
% The probabilities that it returns are not normalised, to avoid evaluating the
% Gamma function (see https://en.wikipedia.org/wiki/Gamma_function).
%
-spec beta_2p_pdf( positive_float_sample(), positive_float(),
				   positive_float() ) -> probability().
beta_2p_pdf( S, Alpha, Beta ) when S >= 0.0 andalso S =< 1.0 ->
	pow( S, Alpha-1.0 ) * pow( 1.0 - S, Beta-1.0 ).



% @doc Checks that the specified term is a sample count (and returns it).
-spec check_sample_count( term() ) -> sample_count().
check_sample_count( C ) when is_integer( C ) andalso C > 0 ->
	C;

check_sample_count( C ) ->
	throw( { invalid_sample_count, C } ).



% @doc Returns a textual representation of the specified random law
% specification.
%
-spec law_spec_to_string( random_law_spec() ) -> ustring().
% In specs, non-canonical types (e.g. integers instead of floats) may be
% encountered, so ~w/~p are more appropriate:
%
law_spec_to_string( { uniform, Max } ) ->
	law_spec_to_string( { uniform, _Min=0.0, Max } );

law_spec_to_string( { uniform, Min, Max } ) ->
	% They may be numbers:
	text_utils:format( "uniform law in [~w,~w]", [ Min, Max ] );

law_spec_to_string( { integer_uniform, Nmax } ) ->
	law_spec_to_string( { integer_uniform, _Nmin=0, Nmax } );

law_spec_to_string( { integer_uniform, Nmin, Nmax } ) ->
	text_utils:format( "integer uniform law in [~w,~w]", [ Nmin, Nmax ] );


law_spec_to_string( { exponential_1p, Lambda } ) ->
	text_utils:format( "exponential-1p law of rate lambda=~w", [ Lambda ] );

law_spec_to_string( { exponential, Lambda } ) ->
	text_utils:format( "exponential law of rate lambda=~w", [ Lambda ] );

law_spec_to_string( { positive_integer_exponential_1p, Lambda } ) ->
	text_utils:format( "integer exponential-1p law of rate lambda=~w",
					   [ Lambda ] );


law_spec_to_string( { exponential_2p, Lambda, Gamma } ) ->
	text_utils:format( "exponential-2p law of rate lambda=~w and gamma=~w",
					   [ Lambda, Gamma ] );

law_spec_to_string( { exponential_2p, Lambda, Gamma, SampleCount } ) ->
	text_utils:format( "exponential-2p law of rate lambda=~w and gamma=~w "
		"(sample count: ~B)", [ Lambda, Gamma, SampleCount ] );

law_spec_to_string( { exponential_2p, Lambda, Gamma, SampleCount, Bounds } ) ->
	text_utils:format( "exponential-2p law of rate lambda=~w and gamma=~w "
		"(sample count: ~B), bounded in ~ts",
		[ Lambda, Gamma, SampleCount, math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { gamma_2p, K, Theta } ) ->
	text_utils:format( "gamma-2p law of shape k=~w and scale theta=~w",
					   [ K, Theta ] );

law_spec_to_string( { gamma_2p, K, Theta, SampleCount } ) ->
	text_utils:format( "gamma-2p law of shape k=~w and scale theta=~w",
		"(sample count: ~B)", [ K, Theta, SampleCount ] );

law_spec_to_string( { gamma_2p, K, Theta, SampleCount, Bounds } ) ->
	text_utils:format( "gamma-2p law of shape k=~w and scale theta=~w ",
		"(sample count: ~B), bounded in ~ts",
		[ K, Theta, SampleCount, math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { gamma_3p, Alpha, Beta, Theta } ) ->
	text_utils:format( "gamma-3p law of shape alpha=~w and beta=~w, "
		"of scale theta=~w", [ Alpha, Beta, Theta ] );

law_spec_to_string( { gamma_3p, Alpha, Beta, Theta, SampleCount } ) ->
	text_utils:format( "gamma-3p law of shape alpha=~w and beta=~w, "
		"of scale theta=~w (sample count: ~B)",
		[ Alpha, Beta, Theta, SampleCount ] );

law_spec_to_string( { gamma_3p, Alpha, Beta, Theta, SampleCount, Bounds } ) ->
	text_utils:format( "gamma-3p law of shape alpha=~w and beta=~w, "
		"of scale theta=~w (sample count: ~B), bounded in ~ts",
		[ Alpha, Beta, Theta, SampleCount,
		  math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { gumbel_2p, Mu, Beta } ) ->
	text_utils:format( "Gumbel-2p law of location parameter mu=~w and "
		"scale parameter betaa=~w", [ Mu, Beta ] );

law_spec_to_string( { gumbel_2p, Mu, Beta, SampleCount } ) ->
	text_utils:format( "Gumbel-2p law of location parameter mu=~w and "
		"scale parameter betaa=~w (sample count: ~B)",
		[ Mu, Beta, SampleCount ] );

law_spec_to_string( { gumbel_2p, Mu, Beta, SampleCount, Bounds } ) ->
	text_utils:format( "Gumbel-2p law of location parameter mu=~w and "
		"scale parameter betaa=~w (sample count: ~B), bounded in ~ts",
		[ Mu, Beta, SampleCount, math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { loglogistic_2p, Alpha, Beta } ) ->
	text_utils:format( "log-logistic-2p law of scale alpha=~w and "
					   "shape beta=~w", [ Alpha, Beta ] );

law_spec_to_string( { loglogistic_2p, Alpha, Beta, SampleCount } ) ->
	text_utils:format( "log-logistic-2p law of scale alpha=~w and "
		"shape beta=~w (sample count: ~B)", [ Alpha, Beta, SampleCount ] );

law_spec_to_string( { loglogistic_2p, Alpha, Beta, SampleCount, Bounds } ) ->
	text_utils:format( "log-logistic-2p law of scale alpha=~w and "
		"shape beta=~w (sample count: ~B), bounded in ~ts",
		[ Alpha, Beta, SampleCount, math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { loglogistic_3p, Alpha, Beta, Theta } ) ->
	text_utils:format( "log-logistic-3p law of scale alpha=~w, "
					   "shape beta=~w and theta=~w", [ Alpha, Beta, Theta ] );

law_spec_to_string( { loglogistic_3p, Alpha, Beta, Theta, SampleCount } ) ->
	text_utils:format( "log-logistic-3p law of scale alpha=~w, "
		"shape beta=~w and theta=~w (sample count: ~B)",
		[ Alpha, Beta, Theta, SampleCount ] );

law_spec_to_string(
		{ loglogistic_3p, Alpha, Beta, Theta, SampleCount, Bounds } ) ->
	text_utils:format( "log-logistic-3p law of scale alpha=~w, "
		"shape beta=~w and theta=~w (sample count: ~B), bounded in ~ts",
		[ Alpha, Beta, Theta, SampleCount,
		  math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { lognormal_2p, Mu, Sigma } ) ->
	text_utils:format( "log-normal-2p law of mu=~w and sigma=~w",
					   [ Mu, Sigma ] );

law_spec_to_string( { lognormal_2p, Mu, Sigma, SampleCount } ) ->
	text_utils:format( "log-normal-2p law of mu=~w and sigma=~w "
					   "(sample count: ~B)", [ Mu, Sigma, SampleCount ] );

law_spec_to_string( { lognormal_2p, Mu, Sigma, SampleCount, Bounds } ) ->
	text_utils:format( "log-normal-2p law of mu=~w and sigma=~w "
		"(sample count: ~B), bounded in ~ts",
		[ Mu, Sigma, SampleCount, math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { lognormal_3p, Mu, Sigma, Theta } ) ->
	text_utils:format( "log-normal-3p law of mu=~w, sigma=~w and theta=~w",
		[ Mu, Sigma, Theta ] );

law_spec_to_string( { lognormal_3p, Mu, Sigma, Theta, SampleCount } ) ->
	text_utils:format( "log-normal-3p law of mu=~w, sigma=~w and theta=~w "
		"(sample count: ~B)", [ Mu, Sigma, Theta, SampleCount ] );

law_spec_to_string(
		{ lognormal_3p, Mu, Sigma, Theta, SampleCount, Bounds } ) ->
	text_utils:format( "log-normal-3p law of mu=~w, sigma=~w and theta=~w"
		"(sample count: ~B), bounded in ~ts",
		[ Mu, Sigma, Theta, SampleCount,
		  math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { gaussian, Mu, Sigma } ) ->
	text_utils:format( "gaussian law of mean mu=~w and standard deviation "
					   "sigma=~w", [ Mu, Sigma ] );

law_spec_to_string( { normal_2p, Mu, Sigma } ) ->
	text_utils:format( "gaussian (normal-2p) law of mean mu=~w and "
		"standard deviation sigma=~w", [ Mu, Sigma ] );

law_spec_to_string( { positive_integer_gaussian, Mu, Sigma } ) ->
	text_utils:format( "positive integer gaussian law of mean mu=~w and "
					   "standard deviation sigma=~w", [ Mu, Sigma ] );


law_spec_to_string( { weibull_2p, K, Lambda } ) ->
	text_utils:format( "Weibull-2p law of shape parameter k=~w and "
		"scale parameter lambda=~w", [ K, Lambda ] );

law_spec_to_string( { weibull_2p, K, Lambda, SampleCount } ) ->
	text_utils:format( "Weibull-2p law of shape parameter k=~w and "
		"scale parameter lambda=~w (sample count: ~B)",
		[ K, Lambda, SampleCount ] );

law_spec_to_string( { weibull_2p, K, Lambda, SampleCount, Bounds } ) ->
	text_utils:format( "Weibull-2p law of shape parameter k=~w and "
		"scale parameter lambda=~w (sample count: ~B), bounded in ~ts",
		[ K, Lambda, SampleCount, math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { weibull_3p, K, Lambda, Gamma } ) ->
	text_utils:format( "Weibull-3p law of shape parameter k=~w, "
		"scale parameter lambda=~w and location parameter gamma=~w",
		[ K, Lambda, Gamma ] );

law_spec_to_string( { weibull_3p, K, Lambda, Gamma, SampleCount } ) ->
	text_utils:format( "Weibull-3p law of shape parameter k=~w, "
		"scale parameter lambda=~w and location parameter gamma=~w"
		"(sample count: ~B)", [ K, Lambda, Gamma, SampleCount ] );

law_spec_to_string( { weibull_3p, K, Lambda, Gamma, SampleCount, Bounds } ) ->
	text_utils:format( "Weibull-3p law of shape parameter k=~w, "
		"scale parameter lambda=~w and location parameter gamma=~w"
		"(sample count: ~B), bounded in ~ts",
		[ K, Lambda, Gamma, SampleCount,
		  math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { weibull_cr, Lambda, K, Theta } ) ->
	text_utils:format( "Weibull-CR law of parameter lambda=~w, "
		"k=~w and theta=~w", [ Lambda, K, Theta ] );

law_spec_to_string( { weibull_cr, Lambda, K, Theta, SampleCount } ) ->
	text_utils:format( "Weibull-CR law of parameter lambda=~w, "
		"k=~w and theta=~w (sample count: ~B)",
		[ Lambda, K, Theta, SampleCount ] );

law_spec_to_string( { weibull_cr, Lambda, K, Theta, SampleCount, Bounds } ) ->
	text_utils:format( "Weibull-CR law of parameter lambda=~w, "
		"k=~w and theta=~w (sample count: ~B), bounded in ~ts",
		[ Lambda, K, Theta, SampleCount,
		  math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { weibull_ds, Lambda, K, Sigma } ) ->
	text_utils:format( "Weibull-DS law of parameter lambda=~w, "
		"k=~w and sigma=~w", [ Lambda, K, Sigma ] );

law_spec_to_string( { weibull_ds, Lambda, K, Sigma, SampleCount } ) ->
	text_utils:format( "Weibull-DS law of parameter lambda=~w, "
		"k=~w and sigma=~w (sample count: ~B)",
		[ Lambda, K, Sigma, SampleCount ] );

law_spec_to_string( { weibull_ds, Lambda, K, Sigma, SampleCount, Bounds } ) ->
	text_utils:format( "Weibull-DS law of parameter lambda=~w, "
		"k=~w and sigma=~w (sample count: ~B), bounded in ~ts",
		[ Lambda, K, Sigma, SampleCount,
		  math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { weibull_dszi, Lambda, K, Sigma, Theta } ) ->
	text_utils:format( "Weibull-DSZI law of parameter lambda=~w, "
		"k=~w, sigma=~w and theta=~w", [ Lambda, K, Sigma, Theta ] );

law_spec_to_string( { weibull_dszi, Lambda, K, Sigma, Theta, SampleCount } ) ->
	text_utils:format( "Weibull-DSZI law of parameter lambda=~w, "
		"k=~w, sigma=~w and theta=~w (sample count: ~B)",
		[ Lambda, K, Sigma, Theta, SampleCount ] );

law_spec_to_string(
		{ weibull_dszi, Lambda, K, Sigma, Theta, SampleCount, Bounds } ) ->
	text_utils:format( "Weibull-DSZI law of parameter lambda=~w, "
		"k=~w, sigma=~w and theta=~w (sample count: ~B), bounded in ~ts",
		[ Lambda, K, Sigma, Theta, SampleCount,
		  math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { weibull_mixture, P, Lambda1, K1, Lambda2, K2 } ) ->
	text_utils:format( "Weibull-Mixture law of parameter p=~w, "
		"lambda1=~w, k1=~w, lambda2=~w, and k2=~w",
		[ P, Lambda1, K1, Lambda2, K2 ] );

law_spec_to_string( { weibull_mixture, P, Lambda1, K1, Lambda2, K2,
					  SampleCount } ) ->
	text_utils:format( "Weibull-Mixture law of parameter p=~w, "
		"lambda1=~w, k1=~w, lambda2=~w, and k2=~w (sample count: ~B)",
		[ P, Lambda1, K1, Lambda2, K2, SampleCount ] );

law_spec_to_string(	{ weibull_mixture, P, Lambda1, K1,
					  Lambda2, K2, SampleCount, Bounds } ) ->
	text_utils:format( "Weibull-Mixture law of parameter p=~w, "
		"lambda1=~w, k1=~w, lambda2=~w, and k2=~w (sample count: ~B), "
		"bounded in ~ts",
		[ P, Lambda1, K1, Lambda2, K2, SampleCount,
		  math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { weibull_zi, Lambda, K, P } ) ->
	text_utils:format( "Weibull-ZI law of parameter lambda=~w, "
		"k=~w and p=~w", [ Lambda, K, P ] );

law_spec_to_string( { weibull_zi, Lambda, K, P, SampleCount } ) ->
	text_utils:format( "Weibull-ZI law of parameter lambda=~w, "
		"k=~w and p=~w (sample count: ~B)",
		[ Lambda, K, P, SampleCount ] );

law_spec_to_string(	{ weibull_zi, Lambda, K, P, SampleCount, Bounds } ) ->
	text_utils:format( "Weibull-ZI law of parameter lambda=~w, "
		"k=~w and p=~w (sample count: ~B), bounded in ~ts",
		[ Lambda, K, P, SampleCount, math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { beta_2p, Alpha, Beta } ) ->
	text_utils:format( "Beta-2p law of shape parameters alpha=~w "
		"and beta=~w", [ Alpha, Beta ] );

law_spec_to_string( { beta_2p, Alpha, Beta, SampleCount } ) ->
	text_utils:format( "Beta-2p law of shape parameters alpha=~w "
		"and beta=~w (sample count: ~B)", [ Alpha, Beta, SampleCount ] );

law_spec_to_string( { beta_2p, Alpha, Beta, SampleCount, Bounds } ) ->
	text_utils:format( "Beta-2p law of shape parameters alpha=~w "
		"and beta=~w (sample count: ~B), bounded in ~ts",
		[ Alpha, Beta, SampleCount, math_utils:bounds_to_string( Bounds ) ] );


law_spec_to_string( { arbitrary, Name, PDFInfo } ) when is_tuple( PDFInfo ) ->
	text_utils:format( "arbitrary law named '~ts', an ~ts",
					   [ Name, pdf_info_to_string( PDFInfo ) ] );

law_spec_to_string( { arbitrary, Name, ProbDist } ) when is_list( ProbDist ) ->
	text_utils:format( "arbitrary law named '~ts', based on a distribution "
					   "of ~B samples", [ Name, length( ProbDist ) ] ).




% @doc Returns a textual representation of the specified random law data.
-spec law_data_to_string( random_law_data() ) -> ustring().
law_data_to_string( { _LawSettings={ uniform, Min, Max },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "uniform law in [~f, ~f]", [ Min, Max ] );

law_data_to_string( { _LawSettings={ integer_uniform, NMin, NMax },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "integer uniform law in [~B, ~B]", [ NMin, NMax ] );


law_data_to_string( { _LawSettings={ exponential_1p, Lambda },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "exponential-1p law of lambda=~f", [ Lambda ] );

law_data_to_string( { _LawSettings={ exponential, Lambda },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "exponential law of lambda=~f", [ Lambda ] );


law_data_to_string( { _LawSettings={ positive_integer_exponential_1p, Lambda },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "integer exponential-1p law of lambda=~f", [ Lambda ] );

law_data_to_string( { _LawSettings={ exponential_2p, Lambda, Gamma,
									 SampleCount, Bounds },
					  _AliasTable } ) ->
	text_utils:format( "exponential-2p law of lambda=~f and gamma=~f, ~ts",
		[ Lambda, Gamma, sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ gamma_2p, K, Theta, SampleCount, Bounds },
					  _MaybeAliasTable } ) ->
	text_utils:format( "Gamma-2p law of k=~f and theta=~f, ~ts",
		[ K, Theta, sampling_info_to_string( SampleCount, Bounds ) ] );

law_data_to_string( { _LawSettings={ gamma_3p, Alpha, Beta, Theta, SampleCount,
									 Bounds }, _MaybeAliasTable } ) ->
	text_utils:format( "Gamma-3p law of alpha=~f, beta=~f and theta=~f, ~ts",
		[ Alpha, Beta, Theta,
		  sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ gumbel_2p, Mu, Beta, SampleCount, Bounds },
					  _MaybeAliasTable } ) ->
	text_utils:format( "Gumbel-2p law of mu=~f and beta=~f, ~ts",
		[ Mu, Beta, sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ loglogistic_2p, Alpha, Beta, SampleCount,
									 Bounds },
					  _MaybeAliasTable } ) ->
	text_utils:format( "Log-logistic-2p law of alpha=~f and beta=~f, ~ts",
		[ Alpha, Beta, sampling_info_to_string( SampleCount, Bounds ) ] );

law_data_to_string( { _LawSettings={ loglogistic_3p, Alpha, Beta, Theta,
							SampleCount, Bounds }, _MaybeAliasTable } ) ->
	text_utils:format( "Loglogistic-3p law of alpha=~f, beta=~f "
		"and theta=~f, ~ts",
		[ Alpha, Beta, Theta,
		  sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ lognormal_2p, Mu, Sigma, SampleCount,
									 Bounds },
					  _MaybeAliasTable } ) ->
	text_utils:format( "Log-normal-2p law of mu=~f and sigma=~f, ~ts",
		[ Mu, Sigma, sampling_info_to_string( SampleCount, Bounds ) ] );

law_data_to_string( { _LawSettings={ lognormal_3p, Mu, Sigma, Theta,
							SampleCount, Bounds }, _MaybeAliasTable } ) ->
	text_utils:format( "Lognormal-3p law of mu=~f, sigma=~f "
		"and theta=~f, ~ts",
		[ Mu, Sigma, Theta,
		  sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ gaussian, Mu, Sigma },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "Gaussian law of mu=~f and sigma=~f", [ Mu, Sigma ] );

law_data_to_string( { _LawSettings={ positive_integer_gaussian, Mu, Sigma },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "integer gaussian law of mu=~f and sigma=~f",
					   [ Mu, Sigma ] );


law_data_to_string( { _LawSettings={ weibull_2p, K, Lambda, SampleCount,
									 Bounds },
					  _MaybeAliasTable } ) ->
	text_utils:format( "Weibull-2p law of k=~f and lambda=~f, ~ts",
		[ K, Lambda, sampling_info_to_string( SampleCount, Bounds ) ] );

law_data_to_string( { _LawSettings={ weibull_3p, K, Lambda, Gamma, SampleCount,
									 Bounds }, _MaybeAliasTable } ) ->
	text_utils:format( "Weibull-3p law of k=~f, lambda=~f and gamma=~f, ~ts",
		[ K, Lambda, Gamma, sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ weibull_cr, Lambda, K, Theta,
						SampleCount, Bounds }, _MaybeAliasTable } ) ->
	text_utils:format( "Weibull-CR law of lambda=~f, k=~f and theta=~f, ~ts",
		[ Lambda, K, Theta, sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ weibull_ds, Lambda, K, Sigma,
						SampleCount, Bounds }, _MaybeAliasTable } ) ->
	text_utils:format( "Weibull-DS law of lambda=~f, k=~f and sigma=~f, ~ts",
		[ Lambda, K, Sigma, sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ weibull_dszi, Lambda, K, Sigma, Theta,
						SampleCount, Bounds }, _MaybeAliasTable } ) ->
	text_utils:format( "Weibull-DSZI law of lambda=~f, k=~f, sigma=~f "
		"and theta=~f, ~ts",
		[ Lambda, K, Sigma, Theta,
		  sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ weibull_mixture, P, Lambda1, K1,
		Lambda2, K2, SampleCount, Bounds }, _MaybeAliasTable } ) ->
	text_utils:format( "Weibull-Mixture law of p=~f, lambda1=~f, k1=~f, "
		"lambda2=~f, k2=~f, ~ts",
		[ P, Lambda1, K1, Lambda2, K2,
		  sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ weibull_zi, Lambda, K, P,
		SampleCount, Bounds }, _MaybeAliasTable } ) ->
	text_utils:format( "Weibull-ZI law of lambda=~f, k=~f and p=~f, ~ts",
		[ Lambda, K, P, sampling_info_to_string( SampleCount, Bounds ) ] );


law_data_to_string( { _LawSettings={ beta_2p, Alpha, Beta, SampleCount,
									 Bounds },
					  _MaybeAliasTable } ) ->
	text_utils:format( "Beta-2p law of alpha=~f, beta=~f, ~ts",
		[ Alpha, Beta, sampling_info_to_string( SampleCount, Bounds ) ] );


% For basic laws:
%
% Typically for discrete distributions:
law_data_to_string( { _LawSettings={ arbitrary, BinName, SampleCount,
									 _MaybeBounds=undefined },
					  _MaybeAliasTable } ) ->
	text_utils:format( "arbitrary law named '~ts', based on ~B samples",
					   [ BinName, SampleCount ] );

law_data_to_string( { _LawSettings={ arbitrary, BinName, SampleCount, Bounds },
					  _MaybeAliasTable } ) ->
	text_utils:format( "arbitrary law named '~ts', ~ts",
		[ BinName, sampling_info_to_string( SampleCount, Bounds ) ] );

law_data_to_string( Other ) ->
	throw( { unexpected_law_data, Other } ).



% @doc Returns a textual representation of the specified PDF information.
-spec pdf_info_to_string( pdf_info() ) -> ustring().
pdf_info_to_string( { _PDF, SampleCount, SampleBounds } ) ->
	text_utils:format( "arbitrary distribution, based on ~B samples, on ~ts",
		[ SampleCount, math_utils:bounds_to_string( SampleBounds ) ] );

pdf_info_to_string( { _PDF, SampleCount } ) ->
	text_utils:format( "arbitrary distribution, based on ~B samples",
					   [ SampleCount ] );

pdf_info_to_string( _PDF ) ->
	"arbitrary distribution".



% @doc Returns a textual representation of the specified sampling information.
-spec sampling_info_to_string( sample_count() ) -> ustring().
sampling_info_to_string( SampleCount ) ->
	text_utils:format( "sampled on ~B points", [ SampleCount ] ).


% @doc Returns a textual representation of the specified sampling information.
-spec sampling_info_to_string( sample_count(), bounds() ) -> ustring().
sampling_info_to_string( SampleCount, Bounds={ StartSample, StopSample } ) ->

	Inc = ( StopSample - StartSample ) / SampleCount,

	text_utils:format( "sampled on ~ts with ~B points "
		"(corresponding increment of ~f)",
		[ math_utils:bounds_to_string( Bounds ), SampleCount, Inc ] ).
