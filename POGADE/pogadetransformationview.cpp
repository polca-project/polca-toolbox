#include "pogadetransformationview.h"
#include "ui_pogadetransformationview.h"

PogadeTransformationView::PogadeTransformationView(QWidget *parent) :
  QWidget(parent),
  ui(new Ui::PogadeTransformationView)
{
  ui->setupUi(this);
}

PogadeTransformationView::~PogadeTransformationView()
{
  delete ui;
}

void PogadeTransformationView::setSourceFile(PogadeProjectSourceFile *sf) {
  _sf = sf;
}

void PogadeTransformationView::updateGUI() {

}
