#ifndef __GL_WIDGET_HPP__
#define __GL_WIDGET_HPP__

#include <QWidget>
#include <QGLViewer/qglviewer.h>

namespace Ui {
  class MainWindow;
}

class GLWidget : public QGLViewer {

Q_OBJECT

public:
  GLWidget(Ui::MainWindow *ui, QWidget *parent=0);
  ~GLWidget();

protected:
  virtual void init();
  virtual void draw();

private:
  Ui::MainWindow *m_ui;

};

#endif //__GL_WIDGET_HPP__
