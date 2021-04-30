
:raw-latex:`\pagebreak`

.. _Maths:


Maths Services
==============

Some simple maths-related operations are defined in the ``myriad/src/maths`` directory:

- the most basic services are centralised in ``math_utils.erl`` and provide:

  - **general operations** apparently lacking to Erlang (for example for conversions or rounding (``floor/1``, ``ceiling/1``), or not exactly implemented as we would have liked (ex: ``modulo/2``)

  - operations tailored to operate on **floating-point values** (ex: ``are_close/{2,3}``, ``are_relatively_close/{2,3}``, ``get_relative_difference/2``, ``is_null/1``)

  - operations on **angles** (ex: ``radian_to_degree/1``, ``canonify/1``)

  - the associated **typing** information

- linear-related operations are defined; for example the **2D** operations are defined in ``linear_2D.erl`` (their **3D** counterparts being defined in ``linear_3D.erl``, their **4D** counterparts in ``linear_4D.erl``; base ones in ``linear.erl``) and include:

  - operations on **points**: ``are_close/2``, ``is_within/3``, ``square_distance/2``, ``distance/2``, ``cross_product/2``, ``roundify/1``, ``get_integer_center/2``, ``get_center/2``, ``translate/2``, etc.

  - operations on **vectors**: ``vectorize/2``, ``square_magnitude/1``, ``magnitude/1``, ``scale/2``, ``make_unit/1``, ``normal_left/1``, ``normal_right/1``, ``dot_product/2``, etc.

  - operations on **lines**: ``get_line/2``, ``intersect/2``, ``get_abscissa_for_ordinate/2``, etc.

  - operations related to **angles**: ``is_strictly_on_the_right/3``, ``is_obtuse/1``, ``abs_angle_rad/3``, ``angle_rad/3``, ``abs_angle_deg/3``, ``angle_deg/3``, etc.

  - operations on **sets of points**: ``compute_smallest_enclosing_rectangle/1``, ``compute_max_overall_distance/1``, ``compute_convex_hull/1``, etc.

- **polygon**-related operations are available in ``polygon.erl``:

  - **generation** of polygons: ``get_triangle/3``, ``get_upright_square/2``, ``get_polygon/1``, etc.

  - **operations** on them: ``get_diameter/1``, ``get_smallest_enclosing_rectangle/1``, ``get_area/1``, ``is_in_clockwise_order/1``, ``is_convex/1``, ``to_string/1``, etc.

  - **rendering** them: ``render/2``, ``set_edge_color/2``, ``get_edge_color/1``, ``set_fill_color/2``, ``get_fill_color/1``, etc.

  - managing their **bounding boxes**: ``update_bounding_box/2``, etc.

- **bounding-boxes in general** are supported in ``bounding_box.erl``, including ``get_lazy_circle_box/1``, ``get_minimal_enclosing_circle_box/1``, etc.

- a minimalist `Runge-Kutta solver <https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods#The_Runge.E2.80.93Kutta_method>`_ is defined in ``rk4_solver.erl``
