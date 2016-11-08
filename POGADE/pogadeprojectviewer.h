#ifndef POGADEPROJECTVIEWER_H
#define POGADEPROJECTVIEWER_H

#include <QWidget>
#include <pogadeproject.h>
#include <QTreeWidgetItem>

namespace Ui {
class PogadeProjectViewer;
}

class PogadeProjectViewer : public QWidget
{
  Q_OBJECT

public:
  explicit PogadeProjectViewer(QWidget *parent = 0, PogadeProject *project = nullptr);
  ~PogadeProjectViewer();
  void setProject(PogadeProject *project = nullptr);

public slots:
  void setGUI();
  void addFile();
  void removeItem();

signals:
  void openSourceFile(PogadeProjectSourceFile *);

private:
  Ui::PogadeProjectViewer *ui;
  PogadeProject *_project = nullptr;
  void addTreeElement(PogadeProjectSourceFile *element);

private slots:
  void fileTreeSelectionChanged();
  void treeDoubleClick(QTreeWidgetItem * item, int column);
  void showFileMenu2(const QPoint& point);
};

#endif // POGADEPROJECTVIEWER_H
