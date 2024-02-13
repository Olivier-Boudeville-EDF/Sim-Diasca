% Copyright (C) 2018-2024 Olivier Boudeville
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
% Creation date: Saturday, February 3, 2018.


-define( rec_base_guard, is_record( Transforms, ast_transforms ) ).

% Useful to trigger an error sooner when debugging:
%-define( rec_guard, when ?rec_base_guard ).
%-define( andalso_rec_guard, andalso ?rec_base_guard ).

% Normal setting:
-define( rec_guard, ).
-define( andalso_rec_guard, ).


% The default in-file location ({Line, Column}) for generated forms:
-define( default_generation_location, {0,1} ).
