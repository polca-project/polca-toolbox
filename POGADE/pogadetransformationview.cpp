#include "pogadetransformationview.h"
#include "ui_pogadetransformationview.h"

#include <vector>
#include <QDebug>
#include <QSettings>
#include <QProcess>
#include <QTreeWidgetItem>
#include <QJsonDocument>
#include <QMessageBox>

#include "pogadedefines.h"
#include "toolswait.h"

PogadeTransformationView::PogadeTransformationView(QWidget *parent) :
  QWidget(parent),
  ui(new Ui::PogadeTransformationView)
{ 
  ui->setupUi(this);
  ui->buttonTransform->setEnabled(false);

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

  ui->treeTransformations->hideColumn(0);
  ui->treeTransformations->setColumnWidth(0, 25);
  ui->treeTransformations->setColumnWidth(1, 25);
  ui->treeTransformations->setColumnWidth(2, 35);

  connect(ui->treeTransformations, SIGNAL(currentItemChanged(QTreeWidgetItem*,QTreeWidgetItem*)),
          this, SLOT(processSelectionChanged(QTreeWidgetItem*,QTreeWidgetItem*)));

  updateGUI();
}

void PogadeTransformationView::processSelectionChanged(int id) {
  ui->buttonTransform->setEnabled(true);
  PolcaTransformation* pt = _sf->transformation(id);
  seNew->setText(pt->codeNew());
  seOld->setText(pt->codeOld());
}

void PogadeTransformationView::processSelectionChanged(QTreeWidgetItem* current, QTreeWidgetItem*) {
  int id = current->text(0).toInt();
  processSelectionChanged(id);
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
  ui->buttonTransform->setEnabled(false);
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

void PogadeTransformationView::transformationUpProcess(int tid) {
  QTreeWidgetItemIterator it(ui->treeTransformations);
  QString id = QString::number(tid);
  while (*it) {
    if ((*it)->text(1) == id) {
      (*it)->setSelected(true);
    }
    else {
      (*it)->setSelected(false);
    }
    ++it;
  }

  processSelectionChanged(tid);
}

void PogadeTransformationView::setTDir(QTemporaryDir *tDir) {
  _tDir = tDir;
}

void PogadeTransformationView::applyTransform() {
  QSettings settings;
  QProcess toolApply;
  QString fileString = _tDir->path() + "/" + _sf->name();

  QStringList argsApply;
  argsApply << fileString;

  QString sid = "";
  QTreeWidgetItemIterator it(ui->treeTransformations);
  while (*it) {
    if((*it)->isSelected()) {
      sid = (*it)->text(1);
    }
    ++it;
  }

  if(sid == "") {
    QMessageBox::critical(this, MYNAME, tr("No Transformation was selected<br>Please Select a transformation to apply"));
  }
  argsApply << sid;
  argsApply << "all";

  toolApply.start(settings.value("PTApply", POLCATOOLAPPLY).toString(), argsApply);

  toolsWait *tw = new toolsWait();
  tw->show();

  if (!toolApply.waitForStarted(-1)) {
    qDebug() << "Wait For Started Reader Failed";
  }

  //qDebug() << "BBB";

  while(!toolApply.waitForFinished(10)) {
    QCoreApplication::processEvents();
    //qDebug() << "Loop";
  }
  delete tw;

  QByteArray _dataApply =  toolApply.readAllStandardOutput();
  QString dataApply = _dataApply;
}

void PogadeTransformationView::processTransformationApplied(QString data) {
  QJsonDocument doc = QJsonDocument::fromJson(data.toLatin1());
  if(doc.isEmpty()) {
    return;
  }
  QJsonObject obj = doc.object();
  QJsonValue newCode = obj.value("code").toString();


  QJsonArray transformations = obj.value("changes").toArray();

  for(QJsonValue t : transformations) {
    PolcaTransformation *pt = new PolcaTransformation;
    pt->setId(PolcaTransformation::idNext());
    PolcaTransformation::idNextIncrease();

    pt->setTransformationId(t.toObject().value("idChange").toInt());
    pt->setLineStart(t.toObject().value("line").toInt());
    pt->setCodeNew(t.toObject().value("newCode").toString());
    pt->setCodeOld(t.toObject().value("oldCode").toString());
    pt->setRuleName(t.toObject().value("ruleName").toString());


    // TODO
    /**********************/
    /**********************/
    //sf->addTransformation(*pt);
    delete pt;
  }

}
