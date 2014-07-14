#include "glew.h"
#include "Logger.hpp"
#include "GLWidget.hpp"

GLWidget::GLWidget(Ui::MainWindow *ui, QWidget *parent)
  : QGLViewer(parent),
    m_ui(ui)
{
}

GLWidget::~GLWidget()
{
}

void GLWidget::init()
{
  ASSERT_MSG(GLEW_OK==glewInit(), "GLWidget: GLEW failed to initialize!");

  glClearColor(0.5f, 0.5f, 0.5f, 1.0f);
}

void GLWidget::draw()
{
}
