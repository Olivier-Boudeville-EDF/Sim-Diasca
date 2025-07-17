/*
 * This is the MyriadGUI base, built-in vertex shader.
 *
 * This shader is able to discover the structure of the vertex attributes
 * it has to process Based on the myriad_gui_vbo_layout uniform variable,
 * and apply it.
 *
 * Each vertex shader instance is to transform exactly one vertex (received
 * from the vertex stream) into another.
 *
 * At least generally, this transformation should behave as a pure
 * (context-free) function.
 *
 * Refer to https://www.khronos.org/opengl/wiki/Vertex_Shader for more
 * information.
 *
 */


/*
 * The GLSL version now matches the one of OpenGL (3.3);
 * using the (default) core profile.
 *
 */
#version 330 core

// MyriadGUI base shader defines:
#include "gui_shader.glsl.h"


/* The conventionally-named uniform variable (see the
 * myriad_gui_vbo_layout_unif_name define) used by MyriadGUI in order that a
 * vertex shader knows the VBO layout, therefore the vertex attributes, that it
 * shall expect:
 *
 */
uniform uint myriad_gui_vbo_layout;


/* The color to be used by VBO layouts not specifying any color (e.g. vtx3):
 *
 * (better be set through a uniform than set as a constant like in:
 * 'myriad_gui_output_color = vec4(0.0, 1.0, 0.0, 1.0);')
 *
 */
uniform vec4 myriad_gui_global_color;


/* Input vertex data, different for all executions of this shader, based on the
 * value to which the myriad_gui_vbo_layout uniform variable above is set by the
 * main program; refer to gui_shader:vbo_layout() for more details.
 *
 * Output is the gl_Position vec4.
 *
 */

// Not using location indices anymore, preferring to use names instead:

/* layout (location = 0) */ in vec3 myriad_gui_input_vertex;
/* layout (location = 1) */ in vec3 myriad_gui_input_normal;
/* layout (location = 2) */ in vec4 myriad_gui_input_color;
/* layout (location = 3) */ in vec2 myriad_gui_input_texcoord;

// Outputs of this shader (inputs of the fragment shader):

out vec4 myriad_gui_current_color;
out vec2 myriad_gui_current_texcoord;


void apply_vtx3() {

	/* This is an identity transformation, basically (so my_input_vertex is
	 * expected to be already in normalized device coordinates):
	 */
	gl_Position.xyz = myriad_gui_input_vertex;

	// For all fragments, the output color will be pure (opaque) green:
	//myriad_gui_current_color = vec4(0.0, 1.0, 0.0, 1.0);

	myriad_gui_current_color = myriad_gui_global_color;

}


void apply_vtx3_rgb() {

	/* This is an identity transformation, basically (so my_input_vertex is
	 * expected to be already in normalized device coordinates):
	 */
	gl_Position.xyz = myriad_gui_input_vertex;

	myriad_gui_current_color = myriad_gui_input_color;
	//myriad_gui_current_color = vec3(0.0, 1.0, 0.0, 1.0);

}


void apply_vtx3_uv() {

	/* This is an identity transformation, basically (so my_input_vertex is
	 * expected to be already in normalized device coordinates):
	 */
	gl_Position.xyz = myriad_gui_input_vertex;

	// Read by the vertex shader in order to forward it to the fragment shader:
	myriad_gui_current_texcoord = myriad_gui_input_texcoord;

}


void main() {

	/* gl_Position is a predefined vec4 output corresponding to the clip-space
	 * output position of the current vertex.
	 *
	 */

	switch (myriad_gui_vbo_layout) {

		case VTX3:
			apply_vtx3();
			break;

		case VTX3_RGB:
			apply_vtx3_rgb();
			break;

		case VTX3_UV:
			apply_vtx3_uv();
			break;

		// All other VBO layouts:
		default:
			break;

	}

	gl_Position.w = 1.0;

}
