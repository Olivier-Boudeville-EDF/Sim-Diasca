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
 */
layout(location = 0) in vec3 my_input_vertex;

void main(){

	/* gl_Position is a predefined vec4 output corresponding to the clip-space
	 * output position of the current vertex.
	 *
	 */

	// This is an identity transformation, basically:
	gl_Position.xyz = my_input_vertex;
	gl_Position.w = 1.0;

}
