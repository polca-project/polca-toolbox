#ifndef POGADESETTINGS_H
#define POGADESETTINGS_H

#include <QDialog>

namespace Ui {
  class pogadeSettings;
}

class pogadeSettings : public QDialog
{
  Q_OBJECT

public:
  explicit pogadeSettings(QWidget *parent = 0);
  ~pogadeSettings();

private slots:
  void chooseToolPR();
  void chooseToolPI();
  void chooseToolPA();
  void chooseToolASMC();
  void doApply();
  void doOK();
  void doCancel();

private:
  Ui::pogadeSettings *ui;
};

#endif // POGADESETTINGS_H
