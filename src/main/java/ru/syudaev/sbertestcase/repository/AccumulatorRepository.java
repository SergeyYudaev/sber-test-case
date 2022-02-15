package ru.syudaev.sbertestcase.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import ru.syudaev.sbertestcase.entity.Accumulator;

/**
 * Репозиторий для {@link Accumulator}
 */
public interface AccumulatorRepository extends JpaRepository<Accumulator, Integer> {

    @Query(value = "" +
            "SELECT * FROM first_table " +
            "ORDER BY random() " +
            "LIMIT 1",
            nativeQuery = true
    )
    Optional<Accumulator> getRandomAccumulatorOpt();
}
