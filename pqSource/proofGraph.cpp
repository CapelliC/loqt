#include <QDebug>
#include "PREDICATE.h"
#include "proofGraph.h"
#include "pqConsole.h"
#include "SwiPrologEngine.h"

proofGraph::proofGraph(QWidget *parent) : lqXDotView(parent) {
    SwiPrologEngine::in_thread _;
    PlCall("set_prolog_flag(pq_tracer, true)");
    PlCall("trace");
    /*
    pqConsole::peek_first()->engine()->query_run("set_prolog_flag(pq_tracer, true)");
    pqConsole::peek_first()->engine()->query_run("trace");
    */
}
#if 0
PREDICATE(pq_trace_interception, 4) {
    const PlTerm
        &Port = PL_A1,
        &Frame = PL_A2,
        &Choice = PL_A3;
    qDebug() << "pq_trace_interception" << t2w(Port) << t2w(Frame) << t2w(Choice);
    /*
    PlTerm Action = PL_A4;
    foreach(t_callback c, debug_callbacks)
        if (c.second(c.first, Port, Frame, Choice, Action)) {
            qDebug() << "action:" << t2w(Action);
            return TRUE;
            //break;
        }
    //qDebug() << "false";
    */
    return FALSE;
}
#endif
