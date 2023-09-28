/*
 * Another simple vertex shader, mostly an identity for the vertex and a
 * pass-through for texture coordinates.
 *
 * Each vertex shader instance is to transform exactly one vertex attribute
 * (received from the vertex stream) into another.
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


/* Input vertex data, different for all executions of this shader;
 * index 0 corresponds to the vector that will be assigned to the
 * user-defined shader input 'my_input_vertex', and 1 corresponds to the
 * associated texture coordinates.
 *
 * Outputs are the gl_Position (as vec4) and the texture coordinates (as vec2).
 *
 * The attributes given to the vertex shader can have "input" in their name, but
 * its output should not include "output" as the fragment shader use the same
 * names and will see these variables as inputs.
 *
 */


// Variables located by name through user vertex attributes:
in vec3 my_input_vertex;
in vec2 my_input_tex_coord;


// Uncomment if not using user vertex attributes:
/*
layout (location = 0) in vec3 my_input_vertex;
layout (location = 1) in vec2 my_input_tex_coord;
layout (location = 2) in mat4 my_model_view_matrix;
*/


out vec2 my_tex_coord;

// The model-view matrix established by the Erlang side of this test:
uniform mat4 my_model_view_matrix;


// The projection matrix cooked by the Erlang side as well:
uniform mat4 my_projection_matrix;


void main(){

	/* gl_Position is a predefined vec4 output corresponding to the clip-space
	 * output position of the current vertex.
	 *
	 */

	/* This is an identity transformation, basically (so my_input_vertex is
	 * expected to be already in normalized device coordinates):
	 */
	gl_Position = my_projection_matrix * my_model_view_matrix * vec4( my_input_vertex, 1.0 );


	// Read by the vertex shader in order to forward it to the fragment shader:
	my_tex_coord = my_input_tex_coord;

}
