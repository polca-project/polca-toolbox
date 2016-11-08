#ifndef POGADEIMPORT_H
#define POGADEIMPORT_H

#include <QDialog>
#include "pogadeproject.h"

namespace Ui {
  class PogadeImport;
}

class PogadeImport : public QDialog
{
  Q_OBJECT

public:
  explicit PogadeImport(QWidget *parent = 0);
  ~PogadeImport();

signals:
  void importProject(PogadeProject *newProject);

private slots:
  void extractProject();
  void selectFile();
  void selectDir();
  void checkInputData();

private:
  Ui::PogadeImport *ui;
};

#endif // POGADEIMPORT_H
