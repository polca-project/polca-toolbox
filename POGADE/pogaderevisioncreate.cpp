#include "pogaderevisioncreate.h"
#include "ui_pogaderevisioncreate.h"

PogadeRevisionCreate::PogadeRevisionCreate(QWidget *parent) :
  QDialog(parent),
  ui(new Ui::PogadeRevisionCreate) {
  ui->setupUi(this);
}

PogadeRevisionCreate::~PogadeRevisionCreate() {
  delete ui;
}

void PogadeRevisionCreate::setFileName(QString name) {
  _name = name;
  ui->labelName->setText(_name);
}

void PogadeRevisionCreate::setNextRevision(unsigned int rev) {
  _rev = rev;
  ui->labelRevision->setText(QString::number(rev));
}

void PogadeRevisionCreate::createRevision() {
  this->accept();
  emit newRevision(ui->lineEditCode->text());
}
