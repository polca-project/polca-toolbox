#include "maximaconector.h"

#include <QDebug>

MaximaConector::MaximaConector(QTemporaryDir *dir)
{
  _tDir = dir;
}

MaximaConector::~MaximaConector()
{

}

bool MaximaConector::isVarLetter(char c) {
  return (c >= 'a' && c <='z') || (c >= 'A' && c <='Z') || c == '_';
}

QStringList findVars(QString line) {
  QStringList list;
  QString word;

  for(int i=0; i<line.size(); i++) {
    char c = line.at(i).toLatin1();
    if(!MaximaConector::isVarLetter(c)) {
      if(word != "") {
        list << word;
        word = "";
      }
    }
    else {
      word = word + c;
    }
  }

  list.removeDuplicates();

  //qDebug() << list;

  return list;
}

bool MaximaConector::simplifyExpression(QString exp, QString &expSimp, QStringList &vars) {
  QString _fileIn  = _tDir->path() + "/mfile.max";
  QString _fileOut = _tDir->path() + "/mfile.out";
  QFile fi(_fileIn);
  QFile fo(_fileOut);

  QString content = "display2d: false;\n";
  content += "a : expand(";
  content += exp;
  content += ");\n";
  content += "stringout(\"";
  content += _fileOut;
  content += "\",a);";

  bool r = fi.open(QFile::WriteOnly | QFile::Text);
  if(r) {
    fi.write(content.toLatin1());
    fi.flush();
    fi.close();
  }
  else
    return false;

  /* maxima --very-quiet -b m.max */
  QStringList args;
  args << "--very-quiet";
  args << "-b";
  args << _fileIn;

  QProcess maxima;

  maxima.start("maxima", args);

  if (!maxima.waitForStarted(-1)) {
    qDebug() << "Wait For Started MAXIMA Failed";
  }

  while(!maxima.waitForFinished(-1)) {
    qDebug() << "Wait For Finish MAXIMA Failed";
  }

  fi.remove();
  QString data;
  r = fo.open(QFile::ReadOnly | QFile::Text);
  if(r) {
    data = fo.readAll();
    fo.close();
    fo.remove();
  }
  else
    return false;

  QStringList dataLines = data.split("\n");

  for(QString line : dataLines) {
    if(line != "") {
      line.chop(1);
      expSimp = line;

      vars = findVars(line);

      return true;
    }
  }

  return true;
}
