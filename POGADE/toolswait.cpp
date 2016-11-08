#include "toolswait.h"
#include "ui_toolswait.h"

toolsWait::toolsWait(QWidget *parent) :
  QDialog(parent),
  ui(new Ui::toolsWait)
{
  ui->setupUi(this);
}

toolsWait::~toolsWait()
{
  delete ui;
}

void toolsWait::cancelTool() {

}
