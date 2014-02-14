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

#include "pqSource.h"
#include "PREDICATE.h"
#include "pqSourceMainWindow.h"
#include "pqTrace.h"
#include "blockSig.h"

#include <QDebug>
#include <QTextStream>
#include <QMessageBox>

/* **** DEBUG **** */

predicate5(goal_source_position)
predicate3(prolog_frame_attribute)

void pqSource::set_action(DebugCommand c, QString a)
{
    debugCommand = c;
    action = a;
    ready.wakeOne();
}

/** start debug mode */
void pqSource::entry_debug_mode(QString query, DebugCommand mode)
{
    Q_UNUSED(mode)
    if (debugStatus == no_Debug) {

        pqTrace::add_debug_callback(this, Trace_);

        if (query.isEmpty() || query == pqSourceMainWindow::emptyQuery())
            query = QFileInfo(file).baseName();

        sendCommand("set_prolog_flag(pq_tracer, true).\n");
        sendCommand(QString("trace,'%1'.\n").arg(query));

        level_curr = 0; // not yet seen
        level_top = 0;
        debugCommand = mode;
        debugStatus = Running;

        emit reportInfo(query);
    }
}

inline T frame_attr(T Frame, QString Attr)
{
    T Value;
    if (prolog_frame_attribute(Frame, A(Attr), Value))
        return Value;
    return T();
}

/* run on Prolog engine thread, wait for user selecting appropriate actions */

bool pqSource::Trace_(QObject *pThis, const T &Port, const T &Frame, const T &Choice, T &Action)
{
    return qobject_cast<pqSource*>(pThis)->Trace_(Port, Frame, Choice, Action);
}

bool pqSource::check_top_level(QString port)
{
    if ((port == "exit" || port == "fail") && level_curr == level_top) {
        level_top = 0;
        debugStatus = no_Debug;
        return false;
    }
    return true;
}

/** called from Prolog execution thread
  */
bool pqSource::Trace_(const T &Port, const T &Frame, const T &Choice, T &Action)
{
    level_curr = frame_attr(Frame, "level");

    QString port = S(Port);
    if (port == "call" && level_top == 0)
        level_top = level_curr;

    long from = 0, stop = 0;

    T goal = frame_attr(Frame, "goal");
    QString s_goal = t2w(goal);

    qDebug() << level_curr << port << s_goal;

    emit reportInfo(QString("(%1) %2 %3").arg(level_curr).arg(port).arg(s_goal));

    QString source;
    T Clause, Position, File;
    if (goal_source_position(Port, Frame, Clause, File, Position)) {
        qDebug() << S(Position) << ',' << S(Choice) << ',' << S(File) << debugStatus;
        source = S(File);
        from = Position[1];
        stop = Position[2];
        if (source != file) {
            qDebug() << "open other file" << source;
        }
    }

    switch (debugCommand) {

    case no_Command:
        break;

    case Run:
        foreach (auto b, bkps)
            if (from == b.beg) {
                setCall(from, stop);
                emit reportInfo(QString("bkp hit %1").arg(s_goal));
                return wait_cmd(Action);
            }
        if (check_top_level(port)) {
            Action = A("continue");
            return true;
        }
        return wait_cmd(Action);

    case StepIn:
        if (check_top_level(port)) {
            Action = A("continue");
            setCall(from, stop);
        }
        return wait_cmd(Action);

    case StepOver:
        if (source == file) {
            setCall(from, stop);
            wait_cmd(Action);
        }
        else
            Action = A("continue");
        return check_top_level(port);

    case StepOut:
        if (source == file) {
            setCall(from, stop);
            wait_cmd(Action);
        }
        else
            Action = A("continue");
        return check_top_level(port);
    }

    return false;
}

/* runtime access */

void pqSource::showCall(long from, long stop)
{
    QTextCursor c = textCursor();
    c.setPosition(from);
    c.movePosition(c.Right, c.KeepAnchor, stop - from);
    setTextCursor(c);
    ensureCursorVisible();
}

void pqSource::sendCommand(QString cmd)
{
    if (deb_server == 0) {
        deb_server = pqConsole::peek_first()->engine();
        connect(deb_server, SIGNAL(query_result(QString,int)), this, SLOT(query_result(QString,int)));
        connect(deb_server, SIGNAL(query_complete(QString,int)), this, SLOT(query_complete(QString,int)));
        connect(deb_server, SIGNAL(query_exception(QString,QString)), this, SLOT(query_exception(QString,QString)));
    }
    sent_commands << cmd;
    deb_server->query_run(cmd);
}

void pqSource::query_result(QString q, int n)
{
    qDebug() << "query_result" << sent_commands << q << n;
}

void pqSource::query_complete(QString q, int n)
{
    qDebug() << "query_complete" << sent_commands << q << n;
    if (sent_commands.count() && sent_commands.takeFirst() == q) {
        emit reportInfo(QString("query_complete %1").arg(q));
        state_curr = state_next;
        state_next = idle;
    }
    else {
        emit reportError(QString("query_complete mismatch %1!=%2").arg(q, sent_commands.join(" :: ")));
        sent_commands.clear();
    }
}

void pqSource::query_exception(QString query, QString message)
{
    qDebug() << "query_exception" << sent_commands << query << message;
    sent_commands.clear();
}

bool pqSource::wait_cmd(PlTerm &Action)
{
    QMutexLocker l(&sync);
    debugStatus = Breaked;
    ready.wait(&sync);
    qDebug() << "wait_cmd done" << action;
    Action = A(action);
    return true;
}

/* top level commands */

bool pqSource::consult()
{
    if (is_modified()) {
        QMessageBox b(this);
        b.setText(tr("'%1' must be saved to run.").arg(file));
        b.setInformativeText(tr("Do you want to save your changes?"));
        b.setStandardButtons(b.Save | b.Cancel);
        b.setDefaultButton(b.Save);
        switch (b.exec()) {
        case b.Cancel:
            return false;
        default:
            return saveSource();
        }
    }

    if (state_curr != compiled) {
        sendCommand(QString("consult('%1')").arg(file));
        state_next = compiled;
        emit reportInfo("consulting");
    }
    else
        emit reportInfo("already consulted");

    return true;
}

void pqSource::stop()
{
    if (debugStatus == Breaked) {
        debugStatus = no_Debug;
        set_action(no_Command, "abort");
        emit reportInfo("Stopped");
    } else if (debugStatus == Running) {
        QMessageBox b(this);
        b.setText(tr("'%1' is running.").arg(file));
        b.setInformativeText(tr("Do you want to stop?"));
        b.setStandardButtons(b.Ok | b.Cancel);
        b.setDefaultButton(b.Ok);
        switch (b.exec()) {
        case b.Ok:
            set_action(no_Command, "abort");
            emit reportInfo("Stopped");
            break;
        default:
            ;
        }
    }
}

void pqSource::run()
{
    if (debugStatus == no_Debug) // && state_curr == compiled)
        entry_debug_mode(findMain()->currentQuery(), Run);
    else if (debugStatus == Breaked) {
        set_action(Run);
        emit reportInfo("Resume Run");
    }
    else
        emit reportError("Cannot run");
}

void pqSource::stepIn()
{
    if (debugStatus == no_Debug) // && state_curr == compiled)
        entry_debug_mode(findMain()->currentQuery(), StepIn);
    else if (debugStatus == Breaked) {
        set_action(StepIn);
        emit reportInfo("stepIn");
    }
    else
         emit reportError("Cannot stepIn");
}

void pqSource::stepOut()
{
    if (debugStatus == Breaked) {
        set_action(StepOut);
        emit reportInfo("stepOut");
    }
    else
        emit reportError("Cannot stepIn");
}

void pqSource::stepOver()
{
    if (debugStatus == Breaked) {
        set_action(StepOver);
        emit reportInfo("stepOver");
    }
    else
        emit reportError("Cannot stepOver");
}

void pqSource::toggleBP()
{
    //sendCommand();

    /*
    if (sd) {
        QTextCursor c = textCursor();
        int p = c.position();
        pqSyntaxData::itcs l = sd->position_path(p);
        if (!l.isEmpty()) {
            const pqSyntaxData::cat &r = *l.back();
            if (QRegExp("goal\\(\\w+\\)").exactMatch(r.desc)) {
                toggle t(skip_changes);
                int q = bkps.indexOf(r);
                if (q >= 0) {
                    bkps.removeAt(q);
                    emit reportInfo(QString("removed BP %1 %2").arg(p).arg(l.size()));
                    r.format(c, r.underline_wave(false));
                }
                else {
                    bkps << r;
                    emit reportInfo(QString("added BP %1 %2").arg(p).arg(l.size()));
                    r.format(c, r.underline_wave(true));
                }
            }
        }
    }
    */
}

void pqSource::watchVar()
{
    /*
    if (sd) {
        pqSyntaxData::itcs l = sd->position_path(textCursor().position());
        if (!l.isEmpty()) {
            emit reportInfo(QString("watchVar %1 %2").arg(textCursor().position()).arg(l.size()));
        }
    }
    */
}

pqSourceMainWindow *pqSource::findMain() const
{
    for (const QWidget *w = this; w; w = w->parentWidget())
        if (auto p = qobject_cast<const pqSourceMainWindow *>(w))
            return const_cast<pqSourceMainWindow *>(p);
    Q_ASSERT(0);
    return 0;
}
