#include "pogademainwindow.h"
#include <QApplication>

#include "pogadedefines.h"

int main(int argc, char *argv[]) {
  QApplication a(argc, argv);

  // Set-up Application Info
  QCoreApplication::setOrganizationDomain(ORGANIZATION_DOMAIN);
  QCoreApplication::setOrganizationName(ORGANIZATION_NAME);
  QCoreApplication::setApplicationName(MYNAME);
  QCoreApplication::setApplicationVersion(VERSION);

  // Load and show the application!
  PogadeMainWindow w;
  w.start();

  return a.exec();
}
