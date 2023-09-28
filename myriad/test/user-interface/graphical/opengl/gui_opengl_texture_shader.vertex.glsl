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
*/


/* Just defined to test the uniform support (will not be found if not explicitly
 * used afterwards):
 *
 */
uniform vec4 some_vector;

//out vec3 my_color;
out vec2 my_tex_coord;


void main(){

	/* gl_Position is a predefined vec4 output corresponding to the clip-space
	 * output position of the current vertex.
	 *
	 */

	/* This is an identity transformation, basically (so my_input_vertex is
	 * expected to be already in normalized device coordinates):
	 */
	gl_Position = vec4( my_input_vertex, 1.0 );

	// Could have been as well:
	/* gl_Position = vec4( my_input_vertex.x, my_input_vertex.y,
	 *                     my_input_vertex.z, 1.0 );
	 */

	//gl_Position = some_vector;

	// Read by the vertex shader in order to forward it to the fragment shader:
	my_tex_coord = my_input_tex_coord;

}
