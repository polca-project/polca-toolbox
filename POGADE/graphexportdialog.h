#ifndef GRAPHEXPORTDIALOG_H
#define GRAPHEXPORTDIALOG_H

#include <QWidget>

#include "vgraph.h"

namespace Ui {
  class GraphExportDialog;
}

class GraphExportDialog : public QWidget
{
  Q_OBJECT

public:
  explicit GraphExportDialog(VGraph* graph, QWidget *parent = 0);
  ~GraphExportDialog();

private slots:
  void chooseFile();
  void exportFile();

private:
  Ui::GraphExportDialog *ui;

  VGraph *_g = nullptr;
};

#endif // GRAPHEXPORTDIALOG_H
