#include "pogadetransformationview.h"
#include "ui_pogadetransformationview.h"

#include <vector>
#include <QDebug>
#include <QTreeWidgetItem>

PogadeTransformationView::PogadeTransformationView(QWidget *parent) :
  QWidget(parent),
  ui(new Ui::PogadeTransformationView)
{ 
  ui->setupUi(this);

  seOld = new SourceCodeEditor();
  seNew = new SourceCodeEditor();

  seOld->setReadOnly(true);
  seNew->setReadOnly(true);

  // Enable Code Highlighting
  QsciLexerCPP *lexer1 = new QsciLexerCPP();
  lexer1->setDefaultFont(seOld->font());
  lexer1->setFoldComments(true);
  seOld->setLexer(lexer1);

  QsciLexerCPP *lexer2 = new QsciLexerCPP();
  lexer2->setDefaultFont(seNew->font());
  lexer2->setFoldComments(true);
  seNew->setLexer(lexer2);

  ui->layoutCodeNew->addWidget(seNew);
  ui->layoutCodeOld->addWidget(seOld);

  connect(ui->treeTransformations, SIGNAL(currentItemChanged(QTreeWidgetItem*,QTreeWidgetItem*)),
          this, SLOT(processSelectionChanged(QTreeWidgetItem*,QTreeWidgetItem*)));
  updateGUI();
}

void PogadeTransformationView::processSelectionChanged(QTreeWidgetItem* current, QTreeWidgetItem*) {
  int id = current->text(0).toInt();

  PolcaTransformation* pt = _sf->transformation(id);
  seNew->setText(pt->codeNew());
  seOld->setText(pt->codeOld());
}

PogadeTransformationView::~PogadeTransformationView()
{
  delete ui;
}

void PogadeTransformationView::setSourceFile(PogadeProjectSourceFile *sf) {
  _sf = sf;
  //updateGUI();
}

void PogadeTransformationView::updateGUI() {
  if(_sf) {
    for(PolcaTransformation pt : _sf->getTransformations()) {
      QTreeWidgetItem *item = new QTreeWidgetItem(ui->treeTransformations);
      item->setText(0, QString::number(pt.id()));
      item->setText(1, QString::number(pt.transformationId()));
      item->setText(2, QString::number(pt.lineStart()));
      item->setText(3, pt.ruleName());
    }
  }
}
