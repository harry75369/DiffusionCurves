#ifndef __GL_WIDGET_HPP__
#define __GL_WIDGET_HPP__

#include <QWidget>
//#include <QGLViewer/qglviewer.h>
#include <QGLWidget>
#include <QOpenGLShaderProgram>

namespace Ui {
  class MainWindow;
}

//class GLWidget : public QGLViewer {
class GLWidget : public QGLWidget {

Q_OBJECT

public:
  GLWidget(Ui::MainWindow *ui, QWidget *parent=0);
  ~GLWidget();

protected: // GL functions
  static void print_glinfo();
  //virtual void init();
  //virtual void draw();
  virtual void initializeGL();
  virtual void resizeGL(int w, int h);
  virtual void paintGL();

private: // UI related
  Ui::MainWindow *m_ui;

private: // GL related
  GLuint m_vao;
  GLuint m_vbo;
  enum LayoutLocation {
    m_vertexLoc,
    m_uvcoordLoc
  };
  QOpenGLShaderProgram m_program;

private: // Some data
  const static int NV = 4;
  GLfloat m_vertices[2*NV];
  GLfloat m_uvcoords[2*NV];
};

#endif //__GL_WIDGET_HPP__
