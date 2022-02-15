package ru.syudaev.sbertestcase.service;

/**
 * Сервис запуска джобов в параллельных потоках.
 */
public interface JobExecutorService {

    /**
     * Инициализирует исходные данные.
     */
    void fillTable();

    /**
     * Запусакет джобы.
     */
    void executeMultiThreadJob();
}
