package ru.syudaev.sbertestcase.service.impl;

import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import ru.syudaev.sbertestcase.entity.Accumulator;
import ru.syudaev.sbertestcase.entity.ThreadMark;
import ru.syudaev.sbertestcase.repository.AccumulatorRepository;
import ru.syudaev.sbertestcase.service.AccumulatorService;

/**
 * Сервис для бработки сущностей типа {@link Accumulator}
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AccumulatorServiceImpl implements AccumulatorService {

    @Value("${application.initial-lines-count}")
    private Integer linesCount;

    private final AccumulatorRepository accumulatorRepository;

    /**
     * Инициализация таблицы (п.1 ТЗ).
     */
    @Override
    public void accumulateInitialData() {
        List<Accumulator> lines = new ArrayList<>();
        for (int i = 0; i < linesCount; i++) {
            lines.add(new Accumulator(null, new ThreadMark()));
        }
        var savedLines = accumulatorRepository.saveAll(lines);
        log.info("Таблица инициализирована, добавлено {} записей: {}", savedLines.size(), savedLines);
    }
}
