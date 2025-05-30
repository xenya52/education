package com.MediaStack.MediaStack.entity.model.mediaFile;

import java.time.LocalDateTime;


import lombok.Getter;
import lombok.Setter;
import lombok.NoArgsConstructor;

import jakarta.persistence.Id;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.validation.constraints.NotBlank;

import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileTypeEnum;

@Entity
@Setter
@Getter
@NoArgsConstructor
/*
  Represents a media file with its properties.
  This class is used to encapsulate the details of a media file such as its ID, name, type, upload date, and path.
 */
public class MediaFileModel {

    @Id
    @GeneratedValue
    String id;

    @NotBlank
    String name;

    MediaFileTypeEnum fileType;

    LocalDateTime uploadDate;

    String path;
}
