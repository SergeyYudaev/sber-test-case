package ru.syudaev.sbertestcase.entity;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Полезная нагрузка для {@link Accumulator}, {@link Result}.
 */
@Data
@AllArgsConstructor
public class ThreadMark {

    /**
     * Признак обработки сущностью нитью исполнения A.
     */
    private Boolean threadA;

    /**
     * Признак обработки сущностью нитью исполнения B.
     */
    private Boolean threadB;

    /**
     * Очередь, в которой запись была обработана потоками.
     */
    private String[] queue;

    public ThreadMark() {
        this.threadA = false;
        this.threadB = false;
        this.queue = new String[2];
    }
}
