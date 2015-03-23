#include "glew.h"
#include "Logger.hpp"
#include "GLWidget.hpp"
#include <QOpenGLShader>

GLWidget::GLWidget(Ui::MainWindow *ui, QWidget *parent)
  //: QGLViewer(parent),
  : QGLWidget(parent),
    m_ui(ui),
    m_program(this),
    m_vertices{-1,-1,-1,1,1,-1,1,1},
    m_uvcoords{0,0,0,1,1,0,1,1}
{
}

GLWidget::~GLWidget()
{
}

void GLWidget::print_glinfo() {
  INFO("Vendor: %s", glGetString(GL_VENDOR));
  INFO("Version: %s", glGetString(GL_VERSION));
  INFO("Renderer: %s", glGetString(GL_RENDERER));
  {
    int nbufs;
    int nsamples;
    glGetIntegerv(GL_SAMPLE_BUFFERS, &nbufs);
    glGetIntegerv(GL_SAMPLES, &nsamples);
    INFO("Sample Buffers: %d", nbufs);
    INFO("Samples: %d", nsamples);
  }
}

//void GLWidget::init()
void GLWidget::initializeGL()
{
  ASSERT_MSG(GLEW_OK==glewInit(), "GLWidget: GLEW failed to initialize!");
  print_glinfo();

  // Some GL inits
  glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);

  // Bind vertex array object
  glGenVertexArrays(1, &m_vao);
  glBindVertexArray(m_vao);

  // Bind buffer
  glGenBuffers(1, &m_vbo);
  glBindBuffer(GL_ARRAY_BUFFER, m_vbo);

  // Use shader
  {
    QOpenGLShader vertexShader(QOpenGLShader::Vertex);
    QOpenGLShader fragmentShader(QOpenGLShader::Fragment);
    vertexShader.compileSourceFile("vertex.glsl");
    fragmentShader.compileSourceFile("fragment.glsl");
    const QString &vlog = vertexShader.log();
    if ( !vlog.isEmpty() ) {
      WARN("Vertex shader compile error: %s", vlog.toStdString().c_str());
    }
    const QString &flog = fragmentShader.log();
    if ( !flog.isEmpty() ) {
      WARN("Fragment shader compile error: %s", flog.toStdString().c_str());
    }
    if ( vertexShader.isCompiled() && fragmentShader.isCompiled() ) {
      m_program.addShader(&vertexShader);
      m_program.addShader(&fragmentShader);
      if ( !m_program.link() ) {
        WARN("Program link error: %s", m_program.log().toStdString().c_str());
      }
      if ( !m_program.bind() ) {
        WARN("Program bind error: %s", m_program.log().toStdString().c_str());
      }
    }
  }

  // Init buffer data and config
  glBufferData(GL_ARRAY_BUFFER, sizeof(m_vertices), NULL, GL_STATIC_DRAW);
  glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(m_vertices), m_vertices);
  glBufferSubData(GL_ARRAY_BUFFER, sizeof(m_vertices), sizeof(m_uvcoords), m_uvcoords);
  glVertexAttribPointer(m_vertexLoc, 2, GL_FLOAT, GL_FALSE, 0, NULL);
  glVertexAttribPointer(m_uvcoordLoc, 2, GL_FLOAT, GL_FALSE, 0, (const GLvoid *)sizeof(m_vertices));
  glEnableVertexAttribArray(m_vertexLoc);
  glEnableVertexAttribArray(m_uvcoordLoc);
}

void GLWidget::resizeGL(int w, int h) {
  glViewport(0, 0, (GLint)w, (GLint)h);
}

//void GLWidget::draw()
void GLWidget::paintGL()
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
}
