/*
    pqSource     : interfacing SWI-Prolog source files and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

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

#ifndef PQSOURCE_H
#define PQSOURCE_H

#include "pqSource_global.h"

#include <QPlainTextEdit>
#include <QTextEdit>
#include <QAction>
#include <QPointer>
#include <QStateMachine>
#include <QElapsedTimer>
#include <QTimer>
#include <QCompleter>

#include "pqConsole.h"
#include "ConsoleEdit.h"
#include "pqHighlighter.h"
#include "pqSourceMainWindow.h"

/* Prolog source editing
   use SWI-Prolog facilities to *edit*, *debug* and *browse*
   relies on pqConsole for initialized syncronous Prolog interface
*/

//typedef QPlainTextEdit pqSourceBaseClass;
typedef QTextEdit pqSourceBaseClass;

class PQSOURCESHARED_EXPORT pqSource : public pqSourceBaseClass {

    Q_OBJECT

//    Q_ENUMS(State)
//    Q_PROPERTY(State state READ getState)

public:

    pqSource(QString file);
    ~pqSource();

    enum State { idle, loaded, isnew, modified, saved, compiled, closing } state_curr, state_next;
    State getState() const { return state_curr; }

    void loadSource(int line = 0, int linepos = 0);
    void placeCursor(int line = 0, int linepos = 0);

    bool saveSource();
    bool saveSourceAs(QString newFile);

    bool canClose();

    QString file;

    bool consult();

    void run();
    void stop();
    void stepIn();
    void stepOut();
    void stepOver();
    void toggleBP();
    void watchVar();

    void insertFromMimeData(const QMimeData *source);
    bool is_modified() const;

    pqHighlighter *semanticHighlighter() const { return hl; }

    void commentClause();

protected:

    QString editWhat;
    QPointer<QAction> editContext, commentContext, renameContext;

    QPointer<pqHighlighter> hl;

    enum DebugStatus { no_Debug, Running, Breaked } debugStatus;
    enum DebugCommand { no_Command, Run, StepIn, StepOver, StepOut } debugCommand;

    QString action;
    long level_min, level_curr, level_top;

    static bool Trace_(QObject *pThis, const PlTerm &, const PlTerm &, const PlTerm &, PlTerm &);
    bool Trace_(const PlTerm &, const PlTerm &, const PlTerm &, PlTerm &);
    bool check_top_level(QString port);
    void set_action(DebugCommand cmd, QString action = "continue");
    bool wait_cmd(PlTerm &Action);
    void entry_debug_mode(QString q, DebugCommand cmd);
    void setCall(long from, long stop) { emit(setCallSig(from, stop)); }

    virtual void closeEvent(QCloseEvent *);
    virtual void keyPressEvent(QKeyEvent *);

    void set_modified(bool yes);

    QStateMachine state_;
    QState created_, syntaxhighlight_, browsing_, modified_;

    bool skip_changes;
    struct toggle {
        toggle(bool &flag, bool value = true) : flag(flag), value(value) {
            flag = value;
        }
        ~toggle() { flag = !value; }
        bool &flag, value;
    };

    // help to know a full parsing is required
    QTimer track_changes;
    int last_change_position;

    QPointer<SwiPrologEngine> deb_server;
    QStringList sent_commands;

    QMutex sync;
    QWaitCondition ready;

    //! breakpoints
    typedef QList<pqSyntaxData::range> t_bkps;
    t_bkps bkps;

    pqSourceMainWindow *findMain() const;

    //! make autocompletion case sensitive, with predicates sorted by - some kind of - proximity
    QPointer<QCompleter> autocomp;

    Q_SLOT void onCompletion(QString completion);
    void completerInit(QTextCursor c);

    void setTitle();
    void reportUser(QString info);

signals:

    void reportInfo(QString info);
    void reportError(QString error);
    void setCallSig(long from, long stop);

    void requestHelp(QString cursorWord);

protected slots:

    void cursorPositionChanged();
    void contentsChange(int position, int charsRemoved, int charsAdded);

    void runHighliter();
    void startHighliter();
    void highlightComplete();

    void showContextMenu(const QPoint &pt);
    void editInvoke();
    void showCall(long from, long stop);

    void sendCommand(QString cmd);

    void query_result(QString,int);
    void query_complete(QString,int);
    void query_exception(QString query, QString message);
};

#endif // PQSOURCE_H
