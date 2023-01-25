/*
    pqConsole    : interfacing SWI-Prolog and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright @ 2023

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

#ifndef PQCONSOLE_H
#define PQCONSOLE_H

#include "pqConsole_global.h"
#include "ConsoleEdit.h"

#include <QMetaObject>
#include <QMetaProperty>
#include <QMutex>

/*!
  \mainpage
    [pqConsole](@ref pqConsole) is an attempt to bring [SWI-Prolog](http://www.swi-prolog.org/) running in [Qt](http://qt-project.org/) environment.

    Using a shared library as basic component, it allows for maximum flexibility in reuse and deployment.
    It's being hosted on [Github](https://github.com/CapelliC/pqConsole).

  \author Carlo Capelli
  \version 0.1

  \class generated by QtCreator for project, this class represent library entry point.
  You can use [ConsoleEdit](@ref ConsoleEdit) *without* instancing pqConsole. Indeed argc/argv are forwarded to ConsoleEdit constructor.

  */
class PQCONSOLESHARED_EXPORT pqConsole {
public:

    /*! Run a vanilla QMainWindow displaying SWI-Prolog console */
    int runDemo(int argc, char *argv[]);

    /** open Prolog script with mini syntax support */
    int showMiniSyntax(int argc, char *argv[]);

#if 0
    /** depth first search of widgets hierarchy, from application topLevelWidgets */
    static QWidget *search_widget(std::function<bool(QWidget* w)> match);
#endif

    /** as noted by Kuba Ober, search_widget() isn't thread safe.
      * Replaced by a list to be safely handled from ConsoleEdit.
      */
    static void addConsole(ConsoleEdit*);
    static void removeConsole(ConsoleEdit*);

    /** search widgets hierarchy looking for the first */
    static ConsoleEdit *by_thread();

    /** search widgets hierarchy looking for any ConsoleEdit */
    static ConsoleEdit *peek_first();

    /** unify a property of QObject */
    static QString unify(const QMetaProperty& p, QObject *o, PlTerm v);

    /** unify a property of QObject, seek by name */
    static QString unify(const char* name, QObject *o, PlTerm v);

    /** run in GUI thread */
    static void gui_run(pfunc f);

    /** saving history lines on exit is messed by PL_halt...*/
    static QStringList last_history_lines;

private:
    static QList<ConsoleEdit*> consoles;
    static QMutex consoles_sync;
};

#endif // PQCONSOLE_H
