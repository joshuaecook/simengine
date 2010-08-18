/*
  % make gui LDLIBS="-lGL -lGLU -lX11 -lXi -lXmu -lglut" CFLAGS="-I/usr/X11/include -L/usr/X11/lib"

 */

#include <stdlib.h>
#include <stdio.h>

#define GL_GLEXT_PROTOTYPES
#include <GL/glut.h>
#include <GL/glext.h>

#define _QUOTE(X) #X
#define Q(X) _QUOTE(X)


const unsigned int WINDOW_WIDTH = 512;
const unsigned int WINDOW_HEIGHT = 512;

int initGL(int,char**);
//- OpenGL callbacks
// Invoked by every iteration of the OpenGL main loop
void display(void);
// Invoked at exit
void cleanup(void);

int main (int argc, char **argv) {
  if (0 != initGL(argc,argv)) {
    return -1;
  }

  glutDisplayFunc(display);


  glutMainLoop();
  return 0;
}



int initGL (int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGBA | GLUT_ALPHA | GLUT_DOUBLE | GLUT_DEPTH);
  glutInitWindowSize(WINDOW_WIDTH, WINDOW_HEIGHT);
  glutCreateWindow("Cuda GL Interop (VBO)");

  // Default to black background
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_ALPHA_TEST);

  glViewport(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT);

  // projection
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluPerspective(60.0, (GLfloat)WINDOW_WIDTH / (GLfloat)WINDOW_HEIGHT, 0.01, 100.0);
  glTranslatef(0.0f, 0.0f, -2.0f);


  atexit(cleanup);

  return 0;
}

void display(void) {
  //glutSwapBuffers();
  //glutPostRedisplay();
}

void cleanup (void) {
}
