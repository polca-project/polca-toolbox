#-------------------------------------------------
#
# Project created by QtCreator 2015-10-06T10:36:24
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = pogade
TEMPLATE = app

unix: LIBS += -lquazip -lz -lqscintilla2
CONFIG += c++11 qscintilla2
unix: CONFIG += link_pkgconfig
unix: PKGCONFIG += ogdf

SOURCES += main.cpp\
        pogademainwindow.cpp \
    pogadeproject.cpp \
    newprojectdialog.cpp \
    pogadeprojectviewer.cpp \
    pogadeprojectsourcefile.cpp \
    pogadesourcecodeeditor.cpp \
    sourcecodeeditor.cpp \
    pogaderevisioncreate.cpp \
    pogadeimport.cpp \
    pogadesettings.cpp \
    pogadegraphviewer.cpp \
    polcascope.cpp \
    polcapragma.cpp \
    vgraph.cpp \
    graphscene.cpp \
    gsnode.cpp \
    pogadescopelist.cpp \
    pggraphicsview.cpp \
    graphexportdialog.cpp \
    toolswait.cpp \
    scopeneighbourinfo.cpp \
    pogadetransformationview.cpp

HEADERS  += pogademainwindow.h \
    pogadeproject.h \
    newprojectdialog.h \
    pogadeprojectviewer.h \
    pogadeprojectsourcefile.h \
    pogadesourcecodeeditor.h \
    sourcecodeeditor.h \
    pogaderevisioncreate.h \
    pogadeimport.h \
    pogadedefines.h \
    pogadesettings.h \
    pogadegraphviewer.h \
    polcascope.h \
    polcapragma.h \
    vgraph.h \
    graphscene.h \
    gsnode.h \
    pogadescopelist.h \
    pggraphicsview.h \
    graphexportdialog.h \
    toolswait.h \
    scopeneighbourinfo.h \
    pogadetransformationview.h

FORMS    += pogademainwindow.ui \
    newprojectdialog.ui \
    pogadeprojectviewer.ui \
    pogadesourcecodeeditor.ui \
    pogaderevisioncreate.ui \
    pogadeimport.ui \
    pogadesettings.ui \
    pogadegraphviewer.ui \
    pogadescopelist.ui \
    graphexportdialog.ui \
    toolswait.ui \
    pogadetransformationview.ui

RESOURCES += \
    appicons.qrc \
    apptranslations.qrc

TRANSLATIONS    += language/pogade_en.ts \
                   language/pogade_es.ts
