/*
 * Most simple vertex shader.
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



/* Input vertex data, different for all executions of this shader;
 * index 0 corresponds to the vector that will be assigned to the
 * user-defined shader input 'my_input_vertex'.
 *
 * Output is the gl_Position vec4.
 *
 */
// Uncomment if not using user vertex attributes:
// layout (location = 0) in vec3 my_input_vertex;

// Comment if using user vertex attributes:
in vec3 my_input_vertex;


/* Just defined to test the uniform support (will not be found if not explicitly
 * used afterwards):
 *
 */
uniform vec4 some_vector;


void main(){

	/* gl_Position is a predefined vec4 output corresponding to the clip-space
	 * output position of the current vertex.
	 *
	 */

	/* This is an identity transformation, basically (so my_input_vertex is
	 * expected to be already in normalized device coordinates):
	 */
	gl_Position.xyz = my_input_vertex;
	gl_Position.w = 1.0;
	//gl_Position = some_vector;

	// Could have been as well:
	/* gl_Position = vec4(my_input_vertex.x, my_input_vertex.y,
	 *                    my_input_vertex.z, 1.0);
	 */

}
