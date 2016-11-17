#ifndef POGADETRANSFORMATIONVIEW_H
#define POGADETRANSFORMATIONVIEW_H

#include <QWidget>
#include <Qsci/qscilexercpp.h>

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

public slots:
  void updateGUI();

private slots:
  void processSelectionChanged(QTreeWidgetItem*,QTreeWidgetItem*);

private:
  Ui::PogadeTransformationView *ui;
  PogadeProjectSourceFile *_sf = nullptr;

  SourceCodeEditor *seOld;
  SourceCodeEditor *seNew;
};

#endif // POGADETRANSFORMATIONVIEW_H
