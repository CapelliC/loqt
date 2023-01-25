/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018,2019

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

#ifndef KEYBOARDMACROS_H
#define KEYBOARDMACROS_H

#include "EditInterface.h"
#include "lqPreferences.h"

#include <QMenu>
#include <QPointer>
#include <QKeyEvent>
#include <QShortcut>
#include <QStatusBar>
#include <QSignalMapper>

/** helper class to record and playback macros
 */
class LQUTYSHARED_EXPORT KeyboardMacros : public QObject
{
    Q_OBJECT
public:

    explicit KeyboardMacros(QObject *parent = 0);
    ~KeyboardMacros();

    /// connect to typical QMainWindow environment
    void setupMenu(QMenu *menu);

    /// resonable defaults for GUI bindings hotkeys
    static QKeySequence start();
    static QKeySequence stop();
    static QKeySequence play();
    static QShortcut *start_(QWidget *parent) { return new QShortcut(start(), parent); }
    static QShortcut *stop_(QWidget *parent) { return new QShortcut(stop(), parent); }
    static QShortcut *play_(QWidget *parent) { return new QShortcut(play(), parent); }

    void manage(QWidget*);

    /// TBD provide named macros with fast startup
    void storeEvent(QKeyEvent *e);
    static QString defaultName() { return "<macro>"; }
    QString currName() const { return defaultName(); }
    void setLastRecordedName(QString name);

    /// serializing to store in preferences
    static QStringList e2l(const QKeyEvent &e);
    static QKeyEvent l2e(const QStringList &l);
    static QString e2s(const QKeyEvent &e) { return e2l(e).join(","); }
    static QKeyEvent s2e(const QString &s) { return l2e(s.split(",")); }

protected:
    bool eventFilter(QObject *obj, QEvent *event);

signals:

    void registerCompleted();
    void playbackCompleted();

    //! basic user feedback about interface status
    void feedback(QString msg);

public slots:

    void startRecording(QWidget*);
    void stopRecording(QWidget*);
    void Playback(QWidget*);

private:
#if 0
    //! restrict to keyboard events by now
    typedef QList<QKeyEvent> macro;
    QMap<QString, macro> macros;
#endif

    //! TBD allows to reference macros by name
    QString lastRecorded;

    QPointer<QSignalMapper> mapStart, mapStop, mapPlay;
    enum { idle, onRecord, onPlayback } status;

    QPointer<QAction>
        macroStartRegAct,
        macroStopRegAct,
        macroPlaybackAct,
        macroSelectAct;
};

#endif // KEYBOARDMACROS_H
