//
//  String+Utils.swift
//  
//
//  Created by Mathew Gacy on 11/23/22.
//

import Foundation

/// Easily throw generic errors with a text description.
extension String: LocalizedError {
    public var errorDescription: String? {
        return self
    }
}
