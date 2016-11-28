#ifndef MAXIMACONECTOR_H
#define MAXIMACONECTOR_H

#include <QFile>
#include <QProcess>
#include <QString>
#include <QStringList>
#include <QTemporaryDir>
#include <QByteArray>

class MaximaConector
{
public:
  MaximaConector(QTemporaryDir *dir);
  ~MaximaConector();

 bool simplifyExpression(QString exp, QString &expSimp, QStringList &vars);

 static bool isVarLetter(char c);

private:
 QTemporaryDir *_tDir;
};

#endif // MAXIMACONECTOR_H
