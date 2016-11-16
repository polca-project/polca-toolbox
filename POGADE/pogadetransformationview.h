#ifndef POGADETRANSFORMATIONVIEW_H
#define POGADETRANSFORMATIONVIEW_H

#include <QWidget>
#include "pogadeprojectsourcefile.h"

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

private:
  Ui::PogadeTransformationView *ui;
  PogadeProjectSourceFile *_sf = nullptr;

};

#endif // POGADETRANSFORMATIONVIEW_H
