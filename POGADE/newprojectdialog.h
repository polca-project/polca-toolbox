#ifndef NEWPROJECTDIALOG_H
#define NEWPROJECTDIALOG_H

#include <QDialog>
#include "pogadeproject.h"

namespace Ui {
class NewProjectDialog;
}

class NewProjectDialog : public QDialog
{
  Q_OBJECT

public:
  explicit NewProjectDialog(QWidget *parent = 0);
  ~NewProjectDialog();

signals:
  void newProjectCreated(PogadeProject *newProject);

private slots:
  void createProject();
  void selectDirectory();

private:
  Ui::NewProjectDialog *ui;
};

#endif // NEWPROJECTDIALOG_H
