#ifndef POLCAPRAGMA_H
#define POLCAPRAGMA_H

#include <QString>

class PolcaPragma
{
public:
  PolcaPragma();
  ~PolcaPragma();

  int lineStart();
  int lineEnd();
  QString text();

  void setLineStart(int line);
  void setLineEnd(int line);
  void setText(QString text);

protected:
  int _lineStart;
  int _lineEnd;
  QString _text;
};

#endif // POLCAPRAGMA_H
