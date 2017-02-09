#include <stdio.h>
#include <QApplication>
#include "gui/MainWindow.hpp"
#include <QGLFormat>

int main(int argc, char * argv[])
{
  QApplication app(argc, argv);
/*
  QGLFormat glf = QGLFormat::defaultFormat();
  glf.setSampleBuffers(true);
  glf.setSamples(4);
  QGLFormat::setDefaultFormat(glf);
*/
  MainWindow window;
  window.show();

  return app.exec();
}
