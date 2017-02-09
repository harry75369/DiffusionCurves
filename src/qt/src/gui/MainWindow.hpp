#ifndef __MAIN_WINDOW_HPP__
#define __MAIN_WINDOW_HPP__

#include <QMainWindow>
#include <QKeyEvent>
#include "GLWidget.hpp"

namespace Ui {
  class MainWindow;
}

class MainWindow : public QMainWindow {

Q_OBJECT

public:
  explicit MainWindow(QWidget *parent=0);
  ~MainWindow();

protected slots:
  void do_actionAbout();
  virtual void keyPressEvent(QKeyEvent *evt);

private:
  Ui::MainWindow *m_ui;
  GLWidget       *m_glwidget;

};

#endif //__MAIN_WINDOW_HPP__
