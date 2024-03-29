% Copyright (C) 2012-2024 EDF R&D

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


% This header file lists common defines and typing shorthand.


% Some shorthands and typing:

-type aggregator_pid() :: class_TraceAggregator:aggregator_pid().

-type agent_pid() :: sim_diasca:agent_pid().


-type time_manager_pid() :: class_TimeManager:time_manager_pid().

-type deployment_manager_pid() :: agent_pid().
% PID of the deployment manager.

-type computing_host_manager_pid() :: agent_pid().
% PID of a computing host manager.


-type load_balancer_pid() :: agent_pid().
% PID of the load balancer.


-type data_exchanger_pid() :: class_DataExchanger:data_exchanger_pid().
% PID of a data exchanger.


-type result_manager_pid() :: agent_pid().
% PID of the result manager.


-type result_producer_pid() :: agent_pid().
% PID of a result producer.


-type probe_ref() :: class_Probe:probe_ref().
% Reference of a probe.


-type probe_pid() :: class_Probe:probe_pid().
% PID of a probe.


-type datalogger_pid() :: class_DataLogger:datalogger_pid().
% PID of the datalogger.


-type web_manager_pid() :: agent_pid().
% PID of the web manager.


-type plugin_manager_pid() :: agent_pid().
% PID of the web manager.


-type performance_tracker_pid() :: agent_pid().
% PID of the performance tracker.


-type instance_tracker_pid() :: agent_pid().
% PID of an instance tracker.


-type resilience_manager_pid() :: class_ResultManager:manager_pid().
% PID of the resilience manager.


-type resilience_agent_pid() :: class_ResilienceAgent:agent_pid().
% PID of a resilience agent.


-type random_manager_pid() :: agent_pid().
% PID of a (mostly deprecated) random manager.


-type trace_aggregator_pid() :: agent_pid().
% PID of a trace aggregator.


% Command-line argument options (as "flags", not associated "values"):

-define( myriad_verbatim_key, '-start-verbatim-options' ).

-define( trace_batch_key, '-batch' ).

-define( engine_arg_root_key, '-sim-diasca-root' ).
-define( engine_arg_version_key, '-sim-diasca-version' ).
