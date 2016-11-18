#ifndef POGADETRANSFORMATIONVIEW_H
#define POGADETRANSFORMATIONVIEW_H

#include <QWidget>
#include <Qsci/qscilexercpp.h>
#include <QTemporaryDir>

#include "pogadeprojectsourcefile.h"
#include "sourcecodeeditor.h"

namespace Ui {
  class PogadeTransformationView;
}

class PogadeTransformationView : public QWidget
{
  Q_OBJECT

public:
  explicit PogadeTransformationView(QWidget *parent = 0);
  ~PogadeTransformationView();
  void setSourceFile(PogadeProjectSourceFile *sf);
  void setTDir(QTemporaryDir *tDir);

public slots:
  void updateGUI();
  void transformationUpProcess(int tid);

private slots:
  void processSelectionChanged(QTreeWidgetItem*,QTreeWidgetItem*);
  void processSelectionChanged(int);
  void applyTransform();

private:
  Ui::PogadeTransformationView *ui;
  PogadeProjectSourceFile *_sf = nullptr;
  SourceCodeEditor *seOld;
  SourceCodeEditor *seNew;
  QTemporaryDir *_tDir;

  void processTransformationApplied(QString data);
};

#endif // POGADETRANSFORMATIONVIEW_H
