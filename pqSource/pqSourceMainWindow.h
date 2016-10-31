/*
    pqSource     : interfacing SWI-Prolog source files and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PQSOURCEMAINWINDOW_H
#define PQSOURCEMAINWINDOW_H

#include <QMainWindow>
#include <QCloseEvent>

#include "pqSource_global.h"
#include "SwiPrologEngine.h"
#include "MdiHelper.h"
#include "KeyboardMacros.h"

class pqSource;
class pqDocView;
class FindReplace;
class proofGraph;

class PQSOURCESHARED_EXPORT pqSourceMainWindow : public MdiHelper
{
    Q_OBJECT
public:
    explicit pqSourceMainWindow(int argc, char **argv, QWidget *parent = 0);

    /** support SWI... prolog_edit:edit_source(File) */
    struct reqEditSource : public QEvent {
        QString file;
        QByteArray geometry;
        int line, linepos;
        reqEditSource(QString file, QByteArray geometry = QByteArray(), int line = 0, int linepos = 0)
            : QEvent(Type(User+1)), file(file), geometry(geometry), line(line), linepos(linepos) {}
    };

    static QString emptyQuery() { return "<editor>"; }
    QString currentQuery() const;

    SwiPrologEngine::in_thread *gui_thread_engine;

    /** get a list of all sources matching bool inspect(pqSource) */
    QList<pqSource*> matching_sources(std::function<bool(pqSource*)> inspect);

    //! fetch the host MainWindow for PlEngines
    static pqSourceMainWindow* hostEngines();

signals:
    void reportErrorSig(QString msg);
    void reportInfoSig(QString msg);

protected:

    virtual void customEvent(QEvent *event);
    virtual void closeEvent(QCloseEvent *e);
    virtual QString symbol(QWidget *w);

    QPointer<FindReplace> findReplace;

    QPointer<QAction> pqWebScriptAct;

    QFile reportFile;
    void reportToFile(QString msg);

    pqDocView *helpView();

    QPointer<KeyboardMacros> macs;

    // only an engine at time, please
    proofGraph* proof = 0;

public slots:

    void openFile(QString p, QByteArray g = QByteArray(), int line = 0, int linepos = 0);

    void newFile();
    void openFile();
    void saveFile();
    void saveFileAs();

    void helpStart();
    void helpDoc();
    void about();

    void find();
    void findNext();
    void findPrevious();
    void replace();

    void reportError(QString msg);
    void reportInfo(QString msg);

    void viewCallGraph();
    void viewGraph();
    void viewInclusions();

    void commentClause();
    void newPublicPred();
    void requestHelp(QString cursorWord);

    void viewSWIPrologPref();
    void selectColors();
    void selectFont();
    void incFont();
    void decFont();
    void incTabs();
    void decTabs();

    void markCursor(QTextCursor c);

    void renderView();
    void renderClause();
    void renderPredicate();

protected slots:

    void fixGeometry();
    void openFileIndex(int index);

    void make();
    void consult();
    void run();
    void stop();
    void stepIn();
    void stepOut();
    void stepOver();
    void toggleBP();
    void watchVar();

    void enableDebug();

    void engine_ready();

    void onWebScript();

    void onFoldClause();
    void onUnfoldClause();
    void onFoldAll();
    void onUnfoldAll();
};

#endif // PQSOURCEMAINWINDOW_H
