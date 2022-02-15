package ru.syudaev.sbertestcase.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.Type;

/**
 * Сущность, являющаяся результатом обработки {@link Accumulator Accumulator-а}.
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "second_table")
public class Result {

    @Id
    private Integer key;

    /**
     * Полезная нагрузка.
     * @see ThreadMark
     */
    @Type(type = "jsonb")
    @Column(columnDefinition = "jsonb")
    private ThreadMark value;

    public static Result from(Accumulator accumulator) {
        return new Result(accumulator.getKey(), accumulator.getValue());
    }
}
