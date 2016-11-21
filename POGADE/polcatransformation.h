#ifndef POLCATRANSFORMATION_H
#define POLCATRANSFORMATION_H

#include <QString>

class PolcaTransformation
{
public:
  PolcaTransformation();
  ~PolcaTransformation();

  void setId(int id);
  int id();

  /***************************************/
  /* STATIC MEMBERS                      */
  /***************************************/
  static int idNext();
  static void idNextReset();
  static void idNextIncrease();
  /***************************************/

  void setTransformationId(int id);
  int transformationId();
  void setLineStart(int line);
  int lineStart();
  void setCodeNew(QString code);
  QString codeNew();
  void setCodeNewAll(QString code);
  QString codeNewAll();
  void setCodeOld(QString code);
  QString codeOld();
  void setRuleName(QString name);
  QString ruleName();

private:
  static int _idCount;
  int _id;

  int _transformationId;
  int _lineStart;
  QString _codeNew;
  QString _codeNewAll;
  QString _codeOld;
  QString _ruleName;
};

#endif // POLCATRANSFORMATION_H
