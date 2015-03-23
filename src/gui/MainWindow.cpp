#include <QMessageBox>
#include "MainWindow.hpp"
#include "ui_MainWindow.h"

MainWindow::MainWindow(QWidget *parent) :
  QMainWindow(parent),
  m_ui(new Ui::MainWindow),
  m_glwidget(new GLWidget(m_ui, this))
{
  m_ui->setupUi(this);

  m_ui->gridLayout->addWidget(m_glwidget, 0, 0);

  connect(m_ui->actionAbout, SIGNAL(triggered()), this, SLOT(do_actionAbout()));
  connect(m_ui->actionQuit, SIGNAL(triggered()), this, SLOT(close()));
}

MainWindow::~MainWindow()
{
  delete m_ui;
  delete m_glwidget;
}

void MainWindow::do_actionAbout()
{
  QMessageBox::information(this, "About", "Author: Chaoya Li\nEmail: harry75369@gmail.com\nDate: July 2014\nQt version: " QT_VERSION_STR);
}

void MainWindow::keyPressEvent(QKeyEvent *evt) {
  if ( evt->key() == Qt::Key_Escape ) {
    this->close();
  }
}
