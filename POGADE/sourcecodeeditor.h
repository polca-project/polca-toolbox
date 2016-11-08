#ifndef SOURCECODEEDITOR_H
#define SOURCECODEEDITOR_H

#include <QObject>
#include <QGraphicsSceneMouseEvent>
#include <x86_64-linux-gnu/qt5/Qsci/qsciscintilla.h>

class SourceCodeEditor : public QsciScintilla
{
  Q_OBJECT

public:
  SourceCodeEditor();
  ~SourceCodeEditor();

signals:
  void doubleClick(int);

private slots:
  void dc(int, int line, int);
};

#endif // SOURCECODEEDITOR_H
