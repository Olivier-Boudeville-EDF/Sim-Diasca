/*
 * This is a pseudo-header file for GLSL, introduced by MyriadGUI.
 *
 *
 */



/* Identifiers of the VBO layouts supported by MyriadGUI.
 *
 * Must be kept consistent with the gui_shader:get_vbo_layout_id/1 function.
 *
 */


// A series of vertex3(); corresponds to the 'vtx3' atom:
const uint VTX3 = 1u;


// A series of (vertex3(), unit_normal3()); corresponds to the 'vtx3_nrm' atom:
const uint VTX3_NRM = 2u;


/* A series of (vertex3(), render_rgb_color()); corresponds to the 'vtx3_rgb'
 * atom:
 *
 */
const uint VTX3_RGB = 3u;


// A series of (vertex3(), uv_point()); corresponds to the 'vtx3_uv' atom:
const uint VTX3_UV = 4u;


/* A series of (vertex3(), unit_normal3(), render_rgb_color()); corresponds to
 * the 'vtx3_nrm_rgb' atom:
 *
 */
const uint VTX3_NRM_RGB = 5u;


/* A series of (vertex3(), unit_normal3(), uv_point()); corresponds to
 * the 'vtx3_nrm_uv' atom:
 *
 */
const uint VTX3_NRM_UV = 6u;
