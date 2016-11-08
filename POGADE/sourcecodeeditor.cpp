#include "sourcecodeeditor.h"

#include <QDebug>

SourceCodeEditor::SourceCodeEditor()
{
  connect(this, SIGNAL(SCN_DOUBLECLICK(int,int,int)),
          this, SLOT(dc(int,int,int)));

}

SourceCodeEditor::~SourceCodeEditor()
{

}

void SourceCodeEditor::dc(int, int line, int)
{
  //qDebug() << "DC: " << line;
  emit doubleClick(line);
}
