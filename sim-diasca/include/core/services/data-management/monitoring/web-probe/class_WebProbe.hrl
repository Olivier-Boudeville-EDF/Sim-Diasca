% Copyright (C) 2019-2026 EDF R&D
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
% Creation date: Tuesday, June 18, 2019.



% Refer to the probe_trace_categorize/1 macro in class_Probe.hrl:
%
% (macro, thus binding no variable)
-define( web_probe_trace_categorize( ProbeTracesInitialisationTermInternal ),

    case ProbeTracesInitialisationTermInternal of

        % Here categorization is already specified:
        % {TraceName, TraceCategorization}, WebProbeOpts} ->
        { { _, _ }, _ } ->
            ProbeTracesInitialisationTermInternal;

        % We have here {TraceName, WebProbeOpts}, where WebProbeOpts is a
        % web_probe_options record; thus, with the web_probe_options record tag,
        % a quadruplet:
        %
        { _, { web_probe_options, _, _, _ } } ->
            % So returning {{TraceName, ?trace_emitter_categorization},
            % WebProbeOpts}:
            { { element( 1, ProbeTracesInitialisationTermInternal ),
                 ?trace_emitter_categorization },
                element( 2, ProbeTracesInitialisationTermInternal ) };

        % We must have here {_TraceName, TraceCategorization}, left as is:
        P={ _, _ } ->
            P;

        % Expecting just TracesStandaloneEmitterName:
        _ ->
            { ProbeTracesInitialisationTermInternal,
              ?trace_emitter_categorization }

    end ).



% Describes options that apply to web probes:
%
% Note that, if changing the number of fields of this record, the
% web_probe_trace_categorize/1 macro above shall be modified accordingly.
%
-record( web_probe_options, {

    % If true, this probe will register itself to the result manager, and be
    % driven by it.
    %
    register_as_tracked_producer = 'true' :: boolean(),


    % Specifies any specific directory path relative to the web root in which
    % the files related to this probe (e.g. *.p, *.data, *.png) should be
    % written.
    %
    probe_directory = undefined :: option( file_utils:directory_name() )

    % Tells whether this web probe requires that extra, local content
    % (e.g. Javascript scripts) can be loaded from the browser:
    %
    % (not used anymore, now that we run our own webserver, instead of tweaking
    % browsers to overcome CORS issues)
    %
    %local_access_required = 'false' :: boolean()

} ).
