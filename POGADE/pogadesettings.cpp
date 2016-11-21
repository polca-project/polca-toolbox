#include "pogadesettings.h"
#include "ui_pogadesettings.h"

#include <QSettings>
#include <QFileDialog>
#include <QDebug>

#include "pogadedefines.h"

pogadeSettings::pogadeSettings(QWidget *parent) :
  QDialog(parent),
  ui(new Ui::pogadeSettings)
{
  QSettings settings;

  ui->setupUi(this);

  // Load Settings
  ui->linePR->setText(settings.value("PTReader", POLCATOOLREADER).toString());
  ui->linePI->setText(settings.value("PTInit", POLCATOOLINIT).toString());
  ui->linePA->setText(settings.value("PTApply", POLCATOOLAPPLY).toString());
  ui->lineASMC->setText(settings.value("PTASMC", POLCATOOLASMC).toString());


  // Load Language Settings
  ui->comboLanguage->addItem(tr("English"), QVariant(QLocale::English));
  ui->comboLanguage->addItem(tr("Spanish"), QVariant(QLocale::Spanish));

  // Set index to current language
  int lang = settings.value("language", QLocale().language()).toInt();
  int index = ui->comboLanguage->findData(QVariant(lang));
  ui->comboLanguage->setCurrentIndex(index);
}

pogadeSettings::~pogadeSettings() {
  delete ui;
}

void pogadeSettings::chooseToolPR() {
  auto fileName = QFileDialog::getOpenFileName(this,
    tr("Open Polca Reader Tool Binary"), "");

  if(!fileName.isEmpty()) {
    ui->linePR->setText(fileName);
  }
}

void pogadeSettings::chooseToolPI() {
  auto fileName = QFileDialog::getOpenFileName(this,
    tr("Open Polca Init Tool Binary"), "");

  if(!fileName.isEmpty()) {
    ui->linePI->setText(fileName);
  }
}

void pogadeSettings::chooseToolPA() {
  auto fileName = QFileDialog::getOpenFileName(this,
    tr("Open Polca Apply Tool Binary"), "");

  if(!fileName.isEmpty()) {
    ui->linePA->setText(fileName);
  }
}

void pogadeSettings::chooseToolASMC() {
  auto fileName = QFileDialog::getOpenFileName(this,
    tr("Open Polca ASMCount Tool file"), "");

  if(!fileName.isEmpty()) {
    ui->lineASMC->setText(fileName);
  }
}

void pogadeSettings::doApply() {
  // TODO: check tools after setting new ones
  qDebug() << "saving Setings "  << ui->comboLanguage->currentData().toInt();

  QSettings settings;
  settings.setValue("PTReader", ui->linePR->text());
  settings.setValue("PTInit", ui->linePI->text());
  settings.setValue("PTApply", ui->linePA->text());
  settings.setValue("PTASMC", ui->lineASMC->text());

  // Save Language Settings
  settings.setValue("language", ui->comboLanguage->currentData().toInt());

  // TODO: change langue in the whole application without restarting
  //qDebug() << "Saved Language";
}

void pogadeSettings::doOK() {
  this->doApply();
  this->close();
  this->~pogadeSettings();
}

void pogadeSettings::doCancel() {
  this->close();
  this->~pogadeSettings();
}
