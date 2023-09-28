% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Wednesday, November 10, 2021.


% Header to export OpenGL-related elements.
%
% One of the purpose is to define them regardless of any specific backend (such
% as wx).
%
% See gui_opengl.erl for the corresponding implementation.


% There are a huge number of defines introduced for OpenGL (more than 5600 of
% them).
%
% wx, quite appropriately, generates them (see lib/wx/include/gl.hrl, generated
% by lib/wx/api_gen/gl_gen{,_erl}.erl); as they are not specific to wx or
% wxWidgets (including reagarding their naming, like 'GL_UNSIGNED_SHORT'), there
% is no point in defining them by ourselves, so MyriadGUI reuses them as they
% are.
%
% Should in the future the wx backend be dropped, either the latest generated
% defines might be hardcoded here or, more probably, the defines of a similar
% backend will be reused.
%
% A problem is that we do not want code relying on Myriad having necessarily to
% depend ultimately on a wx-specific file such as this lib/wx/include/gl.hrl.
%
% So we either include it if available, otherwise we resort to using verbatim a
% duplicated version thereof (which is bound to be {platform,version}-dependent
% and/or to become outdated).
%
% So, knowing that the same applies for glu:

% Currently always expected to be found:
%-if(myriad_gui_backend =:= wxwindows).

% For the various types defined:
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-include("gui_texture.hrl").


% -else.

% If one day these wx-related defines are not available and no other option is
% provided, an hardcoded version of them may have to be placed here.

%-endif.


% The name of the ETS table in charge of storing OpenGL-related versions and
% extensions.
%
-define( gl_info_ets_name, myriad_opengl_info ).
